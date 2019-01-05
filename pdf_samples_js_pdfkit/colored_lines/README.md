```javascript
// create a document and pipe to a blob
var doc = new PDFDocument({autoFirstPage: false, compress: false});
var stream = doc.pipe(blobStream());

doc.addPage(); 

doc.moveTo(150, 150)
   .lineTo(200, 200) 
   .strokeColor('red')
   .stroke()
   .moveTo(210, 200) 
   .lineTo(300, 240) 
   .strokeColor('blue')
   .stroke()
   ;

// end and display the document in the iframe to the right
doc.end();
stream.on('finish', function() {
  iframe.src = stream.toBlobURL('application/pdf');
});
```
