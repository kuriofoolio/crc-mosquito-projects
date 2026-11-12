import Foundation
import PDFKit
import AppKit
let base=URL(fileURLWithPath:FileManager.default.currentDirectoryPath+"/tmp/pdfs/experiment1")
let out=URL(fileURLWithPath:FileManager.default.currentDirectoryPath+"/output/pdf/experiment1")
try FileManager.default.createDirectory(at:out,withIntermediateDirectories:true)
let regex=try NSRegularExpression(pattern:"Mosquito [0-9]+")
let names=["with_deaths","version1","version2","shifted"]
for name in names {
 let original=PDFDocument(url:base.appendingPathComponent(name+".pdf"))!
 let dest=out.appendingPathComponent(name+".pdf")
 var media=original.page(at:0)!.bounds(for:.mediaBox)
 let ctx=CGContext(dest as CFURL,mediaBox:&media,nil)!
 var headers:[String]=[]
 for index in 0..<original.pageCount {
  let p=original.page(at:index)!, text=p.string! as NSString
  let matches=regex.matches(in:text as String,range:NSRange(location:0,length:text.length))
  precondition(matches.count==1,"Expected one plot title on each page")
  let match=matches[0], selection=p.selection(for:match.range)!, rect=selection.bounds(for:p)
  let label="Experiment 1 - "+text.substring(with:match.range)
  headers.append(label)
  ctx.beginPDFPage(nil)
  p.draw(with:.mediaBox,to:ctx)
  ctx.setFillColor(NSColor.white.cgColor)
  ctx.fill(rect.insetBy(dx:-2,dy:-1))
  NSGraphicsContext.saveGraphicsState()
  NSGraphicsContext.current=NSGraphicsContext(cgContext:ctx,flipped:false)
  let attrs:[NSAttributedString.Key:Any]=[.font:NSFont.boldSystemFont(ofSize:rect.height),.foregroundColor:NSColor.black]
  let size=(label as NSString).size(withAttributes:attrs)
  precondition(size.width < media.width-30)
  (label as NSString).draw(at:NSPoint(x:rect.midX-size.width/2,y:rect.midY-size.height/2),withAttributes:attrs)
  NSGraphicsContext.restoreGraphicsState()
  ctx.endPDFPage()
 }
 ctx.closePDF()
 let edited=PDFDocument(url:dest)!
 precondition(edited.pageCount==original.pageCount)
 for i in 0..<edited.pageCount {precondition(edited.page(at:i)!.string!.contains(headers[i]))}
 let thumb=edited.page(at:0)!.thumbnail(of:NSSize(width:1200,height:1200),for:.mediaBox)
 let bitmap=NSBitmapImageRep(data:thumb.tiffRepresentation!)!
 try bitmap.representation(using:.png,properties:[:])!.write(to:base.appendingPathComponent(name+"_after.png"))
 // Check every title visually in a contact sheet, in source page order.
 let sheet=NSImage(size:NSSize(width:900,height:CGFloat(edited.pageCount)*32))
 sheet.lockFocus(); NSColor.white.setFill(); NSRect(x:0,y:0,width:900,height:CGFloat(edited.pageCount)*32).fill()
 for i in 0..<edited.pageCount {
  let p=edited.page(at:i)!, full=p.thumbnail(of:NSSize(width:media.width*2,height:media.height*2),for:.mediaBox)
  let rect=p.bounds(for:.mediaBox)
  let old=original.page(at:i)!, oldtext=old.string! as NSString
  let m=regex.firstMatch(in:oldtext as String,range:NSRange(location:0,length:oldtext.length))!
  let r=old.selection(for:m.range)!.bounds(for:old)
  let crop=NSRect(x:r.midX-170,y:r.midY-11,width:340,height:22)
  let target=NSRect(x:100,y:CGFloat(edited.pageCount-1-i)*32,width:680,height:26)
  full.draw(in:target,from:NSRect(x:crop.minX/rect.width*full.size.width,y:crop.minY/rect.height*full.size.height,width:crop.width/rect.width*full.size.width,height:crop.height/rect.height*full.size.height),operation:.copy,fraction:1)
 }
 sheet.unlockFocus()
 let sb=NSBitmapImageRep(data:sheet.tiffRepresentation!)!
 try sb.representation(using:.png,properties:[:])!.write(to:base.appendingPathComponent(name+"_titles.png"))
 print(name,"verified",edited.pageCount,"titles")
}
