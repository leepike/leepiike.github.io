import os.path
import pathlib
from pathlib import Path
import html


# Generate an index.html file for the directory.

def get_html_files(path):
    html_files = list(Path(path).glob('*'))
    ls = []
    for f in html_files:
        ls.append(f.name)
    return ls

def make_index(path, files):
    """
    Return index.html
    """
    name = "Index of " + path
    header = "<HTML><HEAD>"
    title = "<TITLE>" + name + "</TITLE>"
    header_end = "</HEAD>"
    body = "<BODY>"
    h1 = "<H1>" + name + "</H1>"
    end = "</BODY></HTML>"
    links = []
    for f in files:
        links.append("<A HREF=\"./" + f + "\">" + f + "</A><BR>")
    page = [header, title, header_end, body, h1]
    for l in links:
        page.append(l)
    page.append(end)
    str = '\n'.join(page)
    return str

paths = ['pubs', 'pub_pages', 'talks']
for path in paths:
    print("Processing path: " + path)

    # List all .html files
    files = get_html_files(path)

    # Build index.html
    index = make_index(path, files)

    # Write index file
    filename = os.path.join(path, 'index.html')
    print("Writing file " + filename + "...")
    file = open(filename, 'w')
    file.write(index)
    file.close()
    print("Done.")
