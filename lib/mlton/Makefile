all:

clean:
	find . -type f | egrep '(~$$)|TAGS' | xargs rm -f
	find . -type d | grep 'CM$$' | xargs rm -rf
	rm -f sources.sml mlton.sml
