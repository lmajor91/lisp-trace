SCRIPT_NAME := cl-ptrace.asd
PACKAGE_NAME := :cl-ptrace

target:
	@echo "Run (load-short) to run load the package into the REPL, then (in-package ${PACKAGE_NAME})"
	@sbcl --load ${SCRIPT_NAME} --eval "(asdf:load-system ${PACKAGE_NAME})" --eval "(in-package ${PACKAGE_NAME})"

