NAME := rakuen

# Default rule
default:
	jbuilder build-package $(NAME)

clean:
	rm -rf _build

.PHONY: default clean
