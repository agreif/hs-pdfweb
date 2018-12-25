```javascript
// create a document and pipe to a blob
var doc = new PDFDocument();
var stream = doc.pipe(blobStream());

doc.addPage();
doc.addPage();
doc.addPage();
   
// end and display the document in the iframe to the right
doc.end();
stream.on('finish', function() {
  iframe.src = stream.toBlobURL('application/pdf');
});
```
