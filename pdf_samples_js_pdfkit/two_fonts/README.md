```javascript
// create a document and pipe to a blob
var doc = new PDFDocument({autoFirstPage: false, compress: false});
var stream = doc.pipe(blobStream());

doc.addPage({margin: 200});
doc.font("Helvetica");
doc.text("h");
doc.font("Courier");
doc.text("c");

// end and display the document in the iframe to the right
doc.end();
stream.on('finish', function() {
  iframe.src = stream.toBlobURL('application/pdf');
});
```
