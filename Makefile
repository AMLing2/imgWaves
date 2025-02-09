TARGET = imgwaves
SOURCES = src/imgWaves.lisp src/arg.lisp

all: $(TARGET)

$(TARGET): $(SOURCES)
	sbcl --load src/imgWaves.lisp \
		--eval "(ql:quickload \"IMAGO\")" \
		--eval "(sb-ext:save-lisp-and-die #p\"$(TARGET)\" :toplevel #'start-program :executable t)"
