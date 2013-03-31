MDS = $(wildcard *.md)
HTMLS = $(MDS:%.md=%.html)

all: $(HTMLS)

manual.html: manual.md
	pandoc --standalone \
		--toc \
		--number-sections \
		--css style.css \
		--from markdown+implicit_header_references \
		--to html \
		--output $@ $^

changes.html: changes.md
	pandoc --standalone \
		--css style.css \
		--to html \
		--output $@ $^

clean:
	rm -f $(HTMLS)
