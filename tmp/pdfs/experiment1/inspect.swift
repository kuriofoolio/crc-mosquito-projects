import Foundation
import PDFKit
import AppKit
let url = URL(fileURLWithPath:CommandLine.arguments[1])
let doc=PDFDocument(url:url)!
print("Pages",doc.pageCount)
let p=doc.page(at:0)!
print("Bounds",p.bounds(for:.mediaBox))
let str=p.string! as NSString
for term in ["Mosquito", "Female", "Male"] {
 let r=str.range(of:term)
 if r.location != NSNotFound {print(term,p.selection(for:r)!.bounds(for:p))}
}
let im=p.thumbnail(of:NSSize(width:1000,height:1000),for:.mediaBox)
let b=NSBitmapImageRep(data:im.tiffRepresentation!)!
try b.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:CommandLine.arguments[2]))
