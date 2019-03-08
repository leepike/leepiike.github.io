#!/usr/bin/perl -w

# From http://www.perl.com/pub/a/2001/11/15/creatingrss.html
# Lee Pike
# Generate RSS feed of publications from https://leepike.github.io//

use strict;
use LWP::Simple;
use HTML::TokeParser;
use XML::RSS;

# First - LWP::Simple.  Download the page using get();.
my $content = get( "https://leepike.github.io//" ) or die $!;

# Second - Create a TokeParser object, using our downloaded HTML.
my $stream = HTML::TokeParser->new( \$content ) or die $!;

# Finally - create the RSS object. 
my $rss = XML::RSS->new( version => '0.9' );

# Prep the RSS.
$rss->channel(
	title        => "Lee Pike's Papers",
	link         => "https://leepike.github.io//",
	description  => "Research papers by Lee Pike.");

# Declare variables.
my ($tag, $headline, $url);


# First indication of a headline - A <span> tag is present.
while ( $tag = $stream->get_tag("span") ) {

	# Inside this loop, $tag is at a <span> tag.
        # But do we have a "class="RSS:item">" token, too? 
	if ($tag->[1]{class} and $tag->[1]{class} eq 'RSS:item') {

		# We do! 
                # Get the link.
                $tag = $stream->get_tag('a');

		# Now, we're at the <a> with the title in.
                # We need to put the contents of the 'href' token in $url.
		$url = $tag->[1]{href} || "--";

		# Now we can grab $headline (paper title), 
                # by using get_trimmed_text 
                # up to the close of the <a> tag.
		$headline = $stream->get_trimmed_text('/a'); 

                # We need to escape ampersands, 
                # as they start entity references in XML.
		# $url =~ s/&/&amp;/g;

		# The <a> tags contain relative URLs - 
                # we need to qualify these.
		$url = "https://leepike.github.io//$url";

		# And that's it.  We can add our pair to the RSS channel. 
 		$rss->add_item(title => $headline, 
                               link  => $url);
	    }
    }

$rss->save("pike-pubs.rss");
