use v5.14;

sub parse {
    my $text = shift;

    $text =~ s/{ ( (?: (?> [^{}]+ ) | (?R) )* ) }/
        my $tmpl = $1;
        $tmpl = parse($tmpl) if $tmpl =~ m!{ (?> [^{}]+ ) }!x;
        my @alt = split '\|', $tmpl;
        $alt[rand @alt];
    /xger;
}

#my $input = "Please, design a {function|program} that changes this {{beautiful|awesome} text|template} every time";
chomp(my $input = <>);
say parse($input);
