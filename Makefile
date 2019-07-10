pdf:
	dita -i docsrc/book.ditamap -f pdf --propertyfile=docsrc/pdf.properties -v

html5:
	dita -i docsrc/book.ditamap -f html5 -v
