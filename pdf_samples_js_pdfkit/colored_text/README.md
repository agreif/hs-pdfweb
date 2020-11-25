```javascript
// create a document and pipe to a blob
var doc = new PDFDocument({autoFirstPage: false, compress: false});
var stream = doc.pipe(blobStream());

doc.addPage({margin: 200});
doc
    .fillColor('red')
    .text("abc")
    .fillColor([100, 0, 0, 0])
    .text("def");

// end and display the document in the iframe to the right
doc.end();
stream.on('finish', function() {
  iframe.src = stream.toBlobURL('application/pdf');
});
```
