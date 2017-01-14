all:
	python ./gen.py
	for BUILD in Debug Production; do \
	  for prj in *.gpr; do \
	     echo "$$prj $$BUILD"; \
	     gprbuild -j0 -P $$prj -XBOARD=$$b -XPLATFORM_BUILD=$$BUILD -p -q; \
	  done; \
	done
