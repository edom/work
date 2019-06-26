# on hold
# delay this makefile until build.sh becomes too slow

build_dir = build
src_dir = src

app_src = $(addprefix $(src_dir)/,main.cpp)
lib_src = $(addprefix $(src_dir)/,library.cpp)

# -------------------- infrastructure

.PHONY: all
.PHONY: app
.PHONY: clean
.PHONY: lib

src = $(app_src) $(lib_src)
dep_files = $(addprefix $(build_dir)/,$(addsuffix .d,$(src)))

CXX = clang++
CXXFLAGS = \
    -std=c++11 \
    -fPIC \
    -g \
    -pedantic-errors \
    -Wall \
    -Wextra \
    -Wno-unused-function \
    # -ftime-report \

all: depend lib app

lib:

depend: $(dep_files)

%.h.gch: %.h
	@mkdir -p $(dir $@)
	@echo $(CXX) $(CXXFLAGS) -x c++-header -o $@ $<

$(build_dir)/%.cpp.d: %.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -M -o $@ $<

$(build_dir)/%.h.d: %.h
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -M -o $@ $<

include $(dep_files)
