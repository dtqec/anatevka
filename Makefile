SBCL_BIN=sbcl
SBCL_WORKSPACE?=2048
SBCL_OPTIONS=--noinform --no-userinit --no-sysinit --non-interactive
SBCL=$(SBCL_BIN) --dynamic-space-size $(SBCL_WORKSPACE) $(SBCL_OPTIONS)

# tell me where Quicklisp is
ifeq ($(HOME), /github/home)
	# when running on GitHub Actions, use Docker filesystem location
	QUICKLISP_HOME=/root/quicklisp
else
	QUICKLISP_HOME=$(HOME)/quicklisp
endif
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp

# tell me where local projects are
ifeq ($(HOME), /github/home)
	# when running on GitHub Actions, use Docker filesystem location
	QUICKLISP_PROJECTS=/src
else
	QUICKLISP_PROJECTS=../
endif

QUICKLISP=$(SBCL) --load $(QUICKLISP_SETUP) \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval "(push (truename \"$(QUICKLISP_PROJECTS)\") ql:*local-project-directories*)"

.PHONY: test
test:
	$(QUICKLISP) \
		--eval "(ql:quickload :anatevka-tests)" \
		--eval "(asdf:test-system :anatevka)"

###
### clean targets, borrowed from QVM
###

# Clean the executables
clean:
	rm -f qvm qvm-ng build-output.log system-index.txt

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	$(QUICKLISP) \
		--eval "(ql:register-local-projects)"
	rm -rf "$(LISP_CACHE)"

clean-qvm-cache:
	@echo "Deleting $(QVM_LISP_CACHE)"
	$(QUICKLISP) \
		--eval "(ql:register-local-projects)"
	rm -rf $(QVM_LISP_CACHE)

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) \
		--eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

cleanall: clean clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."

