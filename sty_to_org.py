### My Sty to Org
### v1 created 2022-06-27.
###  By Jake B

# Takes my personal library of LaTeX .sty files and generates a copy suitable
# for use with Orgmode LaTeX exports. Essentially, adds a few header arguments
# determined by a dictionary 'required_headers' and writes the file to a target
# directory in my Emacs folder.

import os
from datetime import datetime

source_dir = "/Users/jake/Dropbox/files/systems/latex/"
target_dir = "/Users/jake/Dropbox/files/systems/emacs-stuff/org/setupfiles_latex/"

files = os.listdir(source_dir)

latex_class = "#+LATEX_CLASS: org-plain-latex\n"
latex_prefix = "#+LATEX_HEADER: "

required_headers = {"jake-speech.sty"     : "#+OPTIONS: toc:nil \n#+LATEX_CLASS_OPTIONS: [letterpaper, 17pt, titlepage] \n ",
                    "jake-journal.sty"    : "#+OPTIONS: toc:nil title:nil num:nil \n#+LATEX_CLASS_OPTIONS: [8pt] \n ",
                    "jake-letter.sty"     : "#+OPTIONS: toc:nil num:nil title:nil \n#+LATEX_CLASS_OPTIONS: [12pt] \n ",
                    "jake-standard.sty"   : "#+LATEX_CLASS_OPTIONS: [letterpaper, 11pt] \n",
                    "jake-quicksport.sty" : "#+OPTIONS: toc:nil \n#+LATEX_CLASS_OPTIONS: [11pt] \n #+LATEX_HEADER: \\author{} \n",
                    "jake-basic-notes.sty": "\n#+OPTIONS: H:5 \n#+LATEX_HEADER: \\author{} \n",
                    "jake-code.sty"       : ""
                    }

no_class = ["jake-code.sty"] # Don't write a class header to these files

generic_header = "This file was automatically created at " + str(datetime.now()) + "\nfrom a matching .sty file " + source_dir

for sty in files:

    if sty in required_headers.keys():

        source_file = open(source_dir + sty, 'r')
        source_content = source_file.readlines()

        target_file = open(target_dir + sty.replace("jake-", "jake-latex-").replace(".sty", "") + ".setup", 'w')

        seen_start = False

        # Write a generic header
        target_file.write(generic_header + sty + "\n\n")   
        # Write out the headers from the dictionary
        target_file.write(required_headers.get(sty) + "\n") 

        # Unless the sty is in the list of files that shouldn't have a class, write out the class
        if not sty in no_class:
            target_file.write(latex_class)

        for i in range(len(source_content)):
            # A like that looks like "% begin jake-speech.sty" denotes the beginning of the file
            if seen_start:
                target_file.write(latex_prefix + source_content[i - 1])
            elif source_content[i].strip() == ("% begin " + sty):
                seen_start = True
            elif source_content[i].strip() == ("% end " + sty):
                seen_start = False
                target_file.write(latex_prefix + source_content[i])

        source_file.close()
        target_file.close()

