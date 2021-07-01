all: schema-bundle.bin

clean:
	rm -f schema-bundle.bin

schema-bundle.bin: schemas/*.prs
	preserves-schemac schemas/*.prs > $@.tmp
	mv $@.tmp $@
