pdf:
	dita -i docsrc/pcl-en-cn.ditamap -f pdf --propertyfile=docsrc/pdf.properties -v

html5:
	dita -i docsrc/pcl-en-cn.ditamap -f html5 -v
