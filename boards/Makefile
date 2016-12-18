ZFP_BOARDS=microbit
RAVENSCAR_BOARDS=crazyflie openmv2 stm32f407disco stm32f429disco stm32f469disco stm32f746disco stm32f769disco

all:
	for BUILD in Debug Production; do \
	  echo "native $$BUILD"; \
	  gprbuild -j0 -P boards.gpr -XBOARD=native -XBUILD=$$BUILD -XRTS=zfp -p; \
	  for b in $(RAVENSCAR_BOARDS); do \
	     echo "$$b ravenscar-sfp $$BUILD"; \
	     gprbuild -j0 -P boards.gpr -XBOARD=$$b -XBUILD=$$BUILD -XRTS=ravenscar-sfp -p; \
	     echo "$$b ravenscar-full $$BUILD"; \
	     gprbuild -j0 -P boards.gpr -XBOARD=$$b -XBUILD=$$BUILD -XRTS=ravenscar-full -p; \
	  done; \
	done
	for d in `find .. -name obj -type d`; do \
	  for sub in $$d/*; do \
	    if [ -d $$sub ]; then \
	      echo '*' > $$sub/.gitignore; \
	      git add -f $$sub/.gitignore; \
	    fi; \
	  done; \
	done
