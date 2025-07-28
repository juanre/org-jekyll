# org-jekyll

**Export Jekyll blog posts from org-mode**

> **Note:** This project is no longer actively maintained (last updated 2014). For current alternatives, please see the [Alternatives](#alternatives) section below.

## Overview

org-jekyll extracts subtrees from your org-publish project files that have a `:blog:` keyword and an `:on:` property with a timestamp, and exports them to a subdirectory `_posts` of your project's publishing directory in the `year-month-day-title.html` format that Jekyll expects. 

Properties are passed over as YAML front-matter in the exported files. The title of the entry is the title of the subtree.

## Documentation

Full documentation is available at: [juanreyero.com/open/org-jekyll/](http://juanreyero.com/open/org-jekyll/)

## Alternatives

Since this project is no longer maintained, you may want to consider these actively maintained alternatives:

### ox-jekyll
- **Repository:** [github.com/mickael-kerjean/ox-jekyll](https://github.com/mickael-kerjean/ox-jekyll)
- **Description:** Org Mode export backend for Jekyll with full YAML front matter support

### org-jekyll-mode
- **Repository:** [github.com/jsuper/org-jekyll-mode](https://github.com/jsuper/org-jekyll-mode)
- **Description:** Emacs extensions for writing Jekyll posts with org-mode

### jekyll-org
- **Repository:** [github.com/cinsk/jekyll-org](https://github.com/cinsk/jekyll-org)
- **Description:** Jekyll with Emacs Org-Mode integration

### Native Org-mode Publishing
Many users now use Org-mode's built-in publishing features directly with Jekyll, often automated through GitHub Actions.

## Author

Juan Reyero - [juanreyero.com](http://juanreyero.com)