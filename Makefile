PROJECT = hwpush

DEPS = jsx

dep_jsx = git git://github.com/talentdeficit/jsx.git v2.6.0

include erlang.mk

SHELL_OPTS = -s hwpush_app start

# Compile flags
#ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

.PHONY: clean_all

clean_all:
	@echo "clean_all start"
	for file_a in `ls ./deps`; do \
	cd ./deps/$$file_a;\
	make clean;\
	cd -;\
	done; \
	make clean
	@echo "clean_all done"

.PHONY: start
start:
	erl -name zhongwencool -setcookie zhongwencool -pa `pwd`/ebin deps/*/ebin  -s hwpush_app



