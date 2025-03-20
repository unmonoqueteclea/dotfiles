#!/usr/bin/env bash

# aliases and functions to deal with llms
# currently, I am using gemini-2.0-flash as my main model

# pipe to this function to visualize nicely markdown
function llm-md() {
  glow -s light "$1"
}


# document these functions
function llm-doc {
  cat ~/vc/dotfiles/shell/.bash/llm.sh | llm -s "Provide a bullet point for each function with a description" | llm-md
}

# llm-denote
# The folder where I store all my notes is called denote, the
# following commands are used to create a RAG system based
# on the llm utility.

# remove the collection, useful when I want to recreate it with last changes
alias llm-denote-clear="llm collections delete denote"
# create the collection, using the E5-large-v2 model
# llm sentence-transformers register intfloat/e5-large-v2 -a lv2
# see https://til.simonwillison.net/llms/embed-paragraphs
alias llm-denote-create="llm embed-multi denote -m lv2 --files ~/Drive/orgmode/denote '**/*.org' --store"

function llm-denote-ask() {
  if [ -z "$1" ]; then
    read -p "What do you want to ask? " question
  else
    question="$1"
  fi
  llm similar denote -c "query: $question" | jq .content | llm  "$question" -s "You answer questions, providing references to the documents where you found the information. Use markdown syntax." | llm-md
}

# functions that extract information from sources to be piped to llm
# see https://til.simonwillison.net/shot-scraper/readability
function llm-pipes-web {
  if [ -z "$1" ]; then
    read -p "What web do you want to read? " web
  else
    web="$1"
  fi
  shot-scraper javascript $web "
    async () => {
      const readability = await import('https://cdn.skypack.dev/@mozilla/readability');
      return (new readability.Readability(document)).parse();
    }" | jq .textContent -r
}

# simpler, just curl the webpage
function llm-pipes-web-html {
  if [ -z "$1" ]; then
    read -p "What web do you want to read? " web
  else
    web="$1"
  fi
  curl -s $web | strip-tags -m;
}

# see https://til.simonwillison.net/llms/rg-pipe-llm-trick
function llm-pipes-rg {
  if [ -z "$1" ]; then
    read -p "What do you want to find? " query
  else
    query="$1"
  fi
  rg -NI -C 10 $query;
}


# combined with "files-to-prompt", we can prompt a whole documentation to llm
function llm-download-web {
  if [ -z "$1" ]; then
    read -p "What web do you want to read? " web
  else
    web="$1"
  fi
  wget --mirror --convert-links --accept=html,htm --no-parent -e robots=off -w 1 -np $web
}

function llm-git-commit-msg {
  git diff --staged | \
    llm -s "Write a succinct commit message. I should consist of a \
      capitalized, short summary, and more detailed explanatory text, \
        if necessary."
}

function llm-explain-py-directory {
  files-to-prompt . -e py | llm -s "Explain the contents of this Python package. Use markdown syntax." | llm-md
}

function llm-explain-py-element {
  symbex $1 | llm --system 'Explain this Python code, succinctly' | llm-md
}
