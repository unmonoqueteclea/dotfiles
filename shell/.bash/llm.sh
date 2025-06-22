#!/usr/bin/env bash

# aliases and functions to deal with llms

# Last full review: 2025-05-25

# main CLI tool: https://github.com/simonw/llm
# installed with uv and using the following plugins:
# llm install llm-sentence-transformers llm-gemini

export LLM_MODEL="gemini-2.0-flash"

# If embeddings fail: llm install llm-sentence-transformers
# see https://til.simonwillison.net/llms/embed-paragraphs

# List of plugins used: llm-tools-sqlite llm-gemini llm-jq llm-sentende-transformers

export LLM_MODEL_EMBEDDINGS="sentence-transformers/intfloat/e5-large-v2"
export LLM_COLLECTION_NOTES="notes"
export LLM_COLLECTION_MEETINGS="meetings"
# 2025-05-25: I could create a collection for journals
export LLM_NOTES_DIR="$HOME/Drive/orgmode/denote"
export LLM_MEETINGS_FOLDER="$HOME/Audio"
export LLM_MEETINGS_NOTES_FOLDER="$HOME/Audio/notes"

# 2025-05-25: This is still the best way to visualize
# llm output markdown nicely. I am following a couple issues in the
# llm repo that could replace this.

# pipe to this function to visualize nicely markdown
function llm-md() { glow -s light "$1"; }

# recreate notes and meetings database
alias llm-note-index="llm collections delete $LLM_COLLECTION_NOTES || \
                       llm embed-multi $LLM_COLLECTION_NOTES \
                                       -m $LLM_MODEL_EMBEDDINGS \
                                        --files $LLM_NOTES_DIR \
                                        '**/*.org' --store"

alias llm-meeting-index="llm collections delete $LLM_COLLECTION_MEETINGS ||\
                        llm embed-multi $LLM_COLLECTION_MEETINGS \
                                       -m $LLM_MODEL_EMBEDDINGS \
                                        --files $LLM_MEETINGS_NOTES_FOLDER \
                                        '**/*.md' --store"

# ask anything to my notes
function llm-note-ask() {
  question=${1:-$(read -p "❓ What do you want to ask? " && echo "$REPLY")}
  llm similar $LLM_COLLECTION_NOTES -n 5 -c "query: $question" | \
    jq -r '. | "\(.id): \(.content)"' | \
    llm  "$question" -s "You answer questions, at the end of the response
                         provide the ids of the documents where you found \
                         the information (just filename, no link). \
                         Use markdown syntax." \
    | llm-md
}

function llm-meeting-ask() {
  question=${1:-$(read -p "❓ What do you want to ask? " && echo "$REPLY")}
  llm similar $LLM_COLLECTION_MEETINGS -n 5 -c "query: $question" | \
    jq -r '. | "\(.id): \(.content)"' | \
    llm  "$question" -s "You answer questions, at the end of the response
                         provide the ids of the documents where you found \
                         the information (just filename, no link). \
                         Use markdown syntax." \
    | llm-md
}

# pipe clean version of webpage to llm
# see https://til.simonwillison.net/shot-scraper/readability
function llm-pipe-web-readable {
  web=${1:-$(read -p "❓ What web do you want to read? " && echo "$REPLY")}
  shot-scraper javascript $web "
    async () => {
      const readability = await import('https://cdn.skypack.dev/@mozilla/readability');
      return (new readability.Readability(document)).parse();
    }" | jq .textContent -r
}

# pipe result of curl to webpage
function llm-pipe-web {
  web=${1:-$(read -p "❓ What web do you want to read? " && echo "$REPLY")}
  curl -s $web | strip-tags -m;
}

# see https://til.simonwillison.net/llms/rg-pipe-llm-trick
function llm-pipe-rg {
  query=${1:-$(read -p "❓ What do you want to find? " && echo "$REPLY")}
  rg -NI -C 10 $query;
}

# grep commit messages to find something and return diffs
function llm-pipe-commits {
  query=${1:-$(read -p "❓ What do you want to find? " && echo "$REPLY")}
  git log -n 100 --grep="$query" --pretty=format:%H | \
    while read commit; do git diff-tree -p $commit; done
}


# combined with "files-to-prompt", we can prompt a whole documentation to llm
function llm-download-web {
  web=${1:-$(read -p "❓ What web do you want to read? " && echo "$REPLY")}
  wget --mirror --convert-links --accept=html,htm --no-parent -e \
       robots=off -w 1 -np $web
}

# write commit message from staged changes
function llm-commit-msg {
  git diff --staged | \
    llm -s "Write a succinct commit message. I should consist of a \
      capitalized, short summary, and more detailed explanatory text, \
      if necessary. Do not use markdown, just plain text. Allowed prefixes: \
      feat, fix, docs, style, refactor test, chore. Do not capitalize prefixes.
      Keep lines short, maximum 80 characters. You can use some emojis, but \
      do not use more than 3. Do not use any other special characters."
}

# extract all python files and ask LLM to explain them
function llm-explain-py-directory {
  files-to-prompt . -e py \
    | llm -s "Explain the contents of this Python package. Use markdown syntax." \
    | llm-md
}

# extract references to a python function or class and ask LLM to explain them
function llm-explain-py-element {
  symbex $1 | llm --system "Explain this Python code, succinctly" | llm-md
}

function llm-meeting-start {
  audio-recorder -c start
}

function llm-meeting-stop {
  new_name=${1:-$(read -p "❓ Name of the meeting? " && echo "$REPLY")}
  audio-recorder -c stop
  cd $LLM_MEETINGS_FOLDER
  local file=$(find "$LLM_MEETINGS_FOLDER" -type f -printf "%T@ %p\n" | sort -n | tail -1 | awk '{print $2}')
  local timestamp=$(date +"%Y%m%d%H%M%S")
  local new_file="$(dirname "$file")/$timestamp-$new_name.mp3"
  echo "Renaming $file to $new_file..."
  mv "$file" "$new_file"
  echo "Writing summary..."
  llm -s "Write the notes of the meeting, a summary of the meeting structured \
         in sections. If needed, add one section with decissions and next actions.\
         Return only the summary, don't add an introduction answering to my question." \
      -a "$new_file"  > "$LLM_MEETINGS_NOTES_FOLDER/$timestamp-$new_name.md"
  echo "Notes saved in notes/$timestamp-$new_name.md"
}

function llm-chat-with-sqlite {
  db_file=${1:-$(read -p "❓ What is the SQLite database file? " && echo "$REPLY")}
  if [[ ! -f "$db_file" ]]; then
    echo "File $db_file does not exist."
    return 1
  fi
  llm chat -T "SQLite(\"$db_file\")"
}
