report.pdf: report.org
	pandoc --toc --number-sections -V documentclass=scrartcl report.org -o report.pdf

report.html: report.org
	pandoc -s report.org -o report.html
