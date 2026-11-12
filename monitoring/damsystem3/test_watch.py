"""Offline tests for the recording safeguards. Does not send notifications."""
import datetime as dt
import pathlib
import subprocess
import tempfile
import unittest

SCRIPT = pathlib.Path(__file__).with_name("dam-watch.zsh").resolve()


def row(age=20, monitor=13, status=1, bad_count=False):
    stamp = dt.datetime.now(dt.timezone.utc) - dt.timedelta(seconds=age)
    fields = ["1", stamp.strftime("%d %b %y"), stamp.strftime("%H:%M:%S"),
              str(status), "0", str(monitor), "0", "Ct", "0", "1"] + ["0"] * 32
    if bad_count:
        fields[-1] = "invalid"
    return "\t".join(fields) + "\r\n"


class RecordingChecks(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = pathlib.Path(self.temp.name)
        self.file = self.base / "Monitor13.txt"
        self.config = self.base / "config.zsh"
        self.file.write_text(row())
        self.configure([self.file], [13])

    def tearDown(self):
        self.temp.cleanup()

    def configure(self, files, ids, extra=""):
        paths = " ".join("'" + str(p).replace("'", "'\\''") + "'" for p in files)
        self.config.write_text(
            f"MONITOR_FILES=({paths})\nMONITOR_IDS=({' '.join(map(str, ids))})\n"
            "DAM_TIMEZONE=UTC\nMAX_DATA_AGE_SECONDS=180\n" + extra)

    def run_watch(self, *args):
        return subprocess.run(["/bin/zsh", "-f", str(SCRIPT), "--config", str(self.config), *args],
                              capture_output=True, text=True, timeout=10)

    def test_fresh_zero_activity_is_healthy_and_default_is_offline(self):
        result = self.run_watch()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("DRY RUN: no network requests", result.stdout)

    def test_stale_data_fails_even_when_file_was_just_written(self):
        self.file.write_text(row(age=600))
        result = self.run_watch()
        self.assertEqual(result.returncode, 1)
        self.assertIn("seconds old", result.stderr)

    def test_missing_file(self):
        self.file.unlink()
        self.assertEqual(self.run_watch().returncode, 1)

    def test_wrong_monitor_number(self):
        self.file.write_text(row(monitor=20))
        self.assertEqual(self.run_watch().returncode, 1)

    def test_invalid_status_is_not_a_healthy_record(self):
        self.file.write_text(row(status=51))
        self.assertEqual(self.run_watch().returncode, 1)

    def test_stale_valid_data_then_fresh_invalid_data_fails(self):
        self.file.write_text(row(age=600) + row(status=51))
        self.assertEqual(self.run_watch().returncode, 1)

    def test_partial_write_does_not_hide_recent_valid_record(self):
        self.file.write_text(row() + "2\t12 Nov")
        self.assertEqual(self.run_watch().returncode, 0)

    def test_bad_counts_are_not_valid(self):
        self.file.write_text(row(bad_count=True))
        self.assertEqual(self.run_watch().returncode, 1)

    def test_future_timestamp_fails(self):
        self.file.write_text(row(age=-600))
        result = self.run_watch()
        self.assertEqual(result.returncode, 1)
        self.assertIn("future", result.stderr)

    def test_every_expected_monitor_must_be_healthy(self):
        other = self.base / "Monitor20.txt"
        other.write_text(row(age=600, monitor=20))
        self.configure([self.file, other], [13, 20])
        result = self.run_watch()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Monitor 20", result.stderr)

    def test_unconfigured_send_is_rejected_before_network(self):
        self.assertEqual(self.run_watch("--send").returncode, 2)

    def test_same_url_for_both_checks_is_rejected_before_network(self):
        url = "https://hc-ping.com/00000000-0000-0000-0000-000000000000"
        self.configure([self.file], [13],
                       f"COMPUTER_PING_URL={url}\nRECORDING_PING_URL={url}\n")
        self.assertEqual(self.run_watch("--send").returncode, 2)


if __name__ == "__main__":
    unittest.main()
