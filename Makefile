# -------------------------------------------------------------------
#
# Copyright (c) 2013-2017 Basho Technologies, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# -------------------------------------------------------------------

prj_dir	:= $(CURDIR)
cache	:= $(prj_dir)/.cache

dl_tgts	:=
#
# tools
#
ifeq ($(REBAR3),)
REBAR3	:= $(cache)/rebar3
dl_tgts	+= $(REBAR3)
endif
export	REBAR3
CP	:= /bin/cp -p
RM	:= /bin/rm

.PHONY	: check clean clean-deps clean-docs clean-dist compile dialyzer \
	  default docs escript prereqs test validate veryclean xref

default : compile

prereqs ::

compile :: prereqs
	$(REBAR3) as prod compile

check :: prereqs
	$(REBAR3) as check do brt-deps --check, dialyzer, xref

clean :: prereqs
	$(REBAR3) clean --all

clean-deps :: clean
	$(RM) -rf $(prj_dir)/_build

clean-docs ::
	$(REBAR3) as docs clean

clean-dist ::
	$(RM) -f $(prj_dir)/cuttlefish

docs :: prereqs
	$(REBAR3) edoc

dialyzer :: prereqs
	$(REBAR3) as check dialyzer

escript :: prereqs
	$(REBAR3) as prod escriptize
	$(CP) _build/prod/bin/cuttlefish $(prj_dir)/cuttlefish

test :: prereqs
	$(REBAR3) eunit

validate :: prereqs
	$(REBAR3) as validate compile

veryclean :: clean clean-docs clean-deps clean-dist
	$(RM) -rf $(cache)

xref :: prereqs
	$(REBAR3) as check xref

#
# how to download files if we need to
#
ifneq ($(dl_tgts),)

dlcmd	:= $(shell which wget 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -O
else
dlcmd	:= $(shell which curl 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -o
else
$(error Need wget or curl to download files)
endif
endif

prereqs :: $(dl_tgts)

veryclean ::
	$(RM) -rf $(dl_tgts)

$(cache)/rebar3 :
	@test -d $(@D) || /bin/mkdir -p $(@D)
	@echo Downloading $@ ...
	@$(dlcmd) $@ https://s3.amazonaws.com/rebar3/rebar3
	@/bin/chmod +x $@

endif	# dl_tgts
