package Wiki::Toolkit::Store::Mediawiki;

use warnings;
use strict;

use Wiki::Toolkit::Store::Database;
use base qw(Wiki::Toolkit::Store::Database);

use Carp qw/carp croak confess/;
use Digest::MD5 qw(md5_hex);
use Time::Piece::Adaptive;
use Time::Seconds;



=head1 NAME

Wiki::Toolkit::Store::Mediawiki - Mediawiki (MySQL) storage backend for
Wiki::Toolkit

=head1 VERSION

Version 0.04

=cut

our $VERSION = '0.04';

=head1 REQUIRES

Subclasses Wiki::Toolkit::Store::Database.

=head1 SYNOPSIS

Implementation of L<Wiki::Toolkit::Store::Database> which reads and writes to a
Mediawiki 1.6 database running in MySQL.  This is module is intended to be
capable of running concurrently with a Mediawiki 1.6 installation without
data corruption.  That said, use it at your own risk.

If you are looking for a general Wiki implementation, you might be better off
looking at L<Wiki::Toolkit::Kwiki>.  It is simpler, more general, and does not
require the database to be initialized by outside software.  Currently,
initializing the database for this module requires a working (PHP) Mediawiki
installation.

I initially wrote this module because I was sick of running both PHP and Perl
on my web server so that I could have the only wiki I could find with the
full featureset I wanted running in parallel with the Perl scripts which
generate the rest of my content dynamically.  Generating my Perl content was
much faster than my Mediawiki installation and I like Perl better, so PHP lost.
Converting the old Mediawiki database into a format that a less fully featured
wiki could read looked generally unrewarding, so here we are.

All date and time values are returned as L<Time::Piece::Adaptive> objects.
This should be transparent for most uses.

See L<Wiki::Toolkit::Store::Database> for more on the general API.

=cut



###
### Globals
###
our $timestamp_fmt = "%Y%m%d%H%M%S";



# Internal method to return the data source string required by DBI.
sub _dsn {
    my ($self, $dbname, $dbhost) = @_;
    my $dsn = "dbi:mysql:$dbname";
    $dsn .= ";host=$dbhost" if $dbhost;
    return $dsn;
}



=head1 METHODS

=head2 check_and_write_node

  $store->check_and_write_node (node     => $node,
				checksum => $checksum,
                                %other_args);

Locks the node, verifies the checksum, calls
C<write_node_post_locking> with all supplied arguments, unlocks the
node. Returns 1 on successful writing, 0 if checksum doesn't match,
croaks on error.

Note:  Uses MySQL's user level locking, so any locks are released when
the database handle disconnects.  Doing it like this because I can't seem
to get it to work properly with transactions.

=cut

sub check_and_write_node
{
    my ($self, %args) = @_;
    my ($node, $checksum) = @args{qw(node checksum)};
    $self->_lock_node ($node) or croak "Can't lock node";
    my $ok = $self->verify_checksum ($node, $checksum);
    unless ($ok)
    {
        $self->_unlock_node ($node) or carp "Can't unlock node";
	return 0;
    }
    eval {$self->write_node_post_locking (%args)};
    my $saverr = $@;
    $self->_unlock_node ($node) or carp "Can't unlock node";
    croak $saverr if $saverr;
    return 1;
}



=head2 new

Like the C<new> function from C<Wiki::Toolkit::Store::MySQL>, but also requires
a `wikiname' argument.

=cut

sub new {
    my ($class, %args) = @_;
    my $self = {};
    bless $self, $class;

    # wikiname is required
    croak "missing required `wikiname' argument" unless $args{wikiname};
    $self->{wikiname} = $args{wikiname};

    # Set defaults for these arguments.
    if (exists $args{convert_spaces}) {
	$self->{convert_spaces} = $args{convert_spaces};
    } else {
	$self->{convert_spaces} = 1;
    }

    $self->{default_date_format} = $args{default_date_format}
	if $args{default_date_format};

    if (exists $args{ignore_case}) {
	$self->{ignore_case} = $args{ignore_case};
    } else {
	$self->{ignore_case} = 1;
    }

    # Call the parent initializer.
    return $self->_init (%args);
}

# Returns 1 if we can get a lock, 0 if we can't, croaks on error.
sub _lock_node
{
    my ($self, $node) = @_;
    my $dbh = $self->{_dbh};
    $node = $dbh->quote ($node);
    my $sql = "SELECT GET_LOCK($node, 10)";
    my $sth = $dbh->prepare($sql);
    $sth->execute or croak $dbh->errstr;
    my $locked = $sth->fetchrow_array;
    $sth->finish;
    return $locked;
}

# Returns 1 if we can unlock, 0 if we can't, croaks on error.
sub _unlock_node {
    my ($self, $node) = @_;
    my $dbh = $self->{_dbh};
    $node = $dbh->quote($node);
    my $sql = "SELECT RELEASE_LOCK($node)";
    my $sth = $dbh->prepare($sql);
    $sth->execute or croak $dbh->errstr;
    my $unlocked = $sth->fetchrow_array;
    $sth->finish;
    return $unlocked;
}



our @namespaces = qw{Talk User User_talk Project Project_talk Image Image_talk
		     MediaWiki MediaWiki_talk Template Template_talk Help
		     Help_talk Category Category_talk};
		
# $store->__namespace_to_num ($node_name);
#
# Translate a node name containing a `:' into a Mediawiki namespace number.
sub __namespace_to_num
{
    my ($self, $name) = @_;
    $name =~ s/ /_/g if $self->{convert_spaces};
    return 0, $name unless $name =~ /^(?::+)?([^:]+):+([^:].*)$/;
    return -2, $2 if $1 eq 'Media';
    return -1, $2 if $1 eq 'Special';
    return 4, $2 if $1 eq $self->{wikiname};
    for (0 .. $#namespaces)
    {
	return $_ + 1, $2 if $1 eq $namespaces[$_];
    }
    return 0, $name;
}



# $store->__num_to_namespace ($namespace_code, $node_name);
#
# Translate a Mediawiki namespace number into a node name containing a `:'.
sub __num_to_namespace
{
    my ($self, $num, $name) = @_;
    $name =~ s/_/ /g if $self->{convert_spaces};
    return $name unless $num;
    return "Media:$name" if $num == -2;
    return "Special:$name" if $num == -1;
    return $self->{wikiname} . ":$name" if $num == 4;
    die "no such namespace $num"
	unless $num > 0 && $num < @namespaces;
    return "$namespaces[$num - 1]:$name";
}



# turn the Wiki::Toolkit metadata fields of a search into a metadata hash
# substructure.
my @metadata_fields = qw{comment edit_type patrolled username};
sub _make_metadata
{
    my $data = shift;
    my %metadata;
    @metadata{@metadata_fields} = map { [$_] } @$data{@metadata_fields};
    $data->{metadata} = \%metadata;
}



sub _make_date
{
    my ($self, $date) = @_;
    my $newdate;
    my @strptime_args = ($date ? $date : "19700101000000", $timestamp_fmt);
    push @strptime_args, stringify => $self->{default_date_format}
	if $self->{default_date_format};
    eval {
	$newdate = Time::Piece::Adaptive->strptime (@strptime_args);
    };
    croak "bad timestamp (`$date').\n", $@ if $@;
    return $newdate;
}



# Returns hash or scalar depending on calling context.
sub _retrieve_node_data
{
    my ($self, %args) = @_;
    croak "Need name or version to lookup node"
	unless $args{name} || $args{version};
    my $dbh = $self->dbh;
    my $sql;
    my %data;
    my @outfields = qw{content last_modified};
    my $infields;
    my $ignore_case = defined $args{ignore_case}
		      ? $args{ignore_case} : $self->{ignore_case};
    if ($args{version})
    {
	$infields = "old_text";
	if (wantarray)
	{
	  push @outfields, qw{ns name};
	  $infields .= ", old_timestamp, old_namespace, old_title";
	  unless ($args{nometadata})
	  {
	    push @outfields, qw{edit_type username comment};
	    $infields .= ", old_minor_edit, old_user_text, old_comment";
	  }
	}
	$data{version} = $args{version} if $args{version};
        $sql = "SELECT $infields"
	       . " FROM text"
               . " WHERE old_id="
	       . $dbh->quote ($args{version});
    }
    else
    {
	my ($ns, $name) = $self->__namespace_to_num ($args{name});
	$infields = "cur_text";
	if (wantarray)
	{
	  push @outfields, qw{version};
	  $infields .= ", page_touched, page_latest";
	  if ($ignore_case)
	  {
	    push @outfields, qw{ns name};
	    $infields .= ", page_namespace, page_title";
	  }
	  unless ($args{nometadata})
	  {
	    push @outfields, qw{edit_type username comment};
	    $infields .= ", cur_minor_edit, cur_user_text, cur_comment";
	  }
	}
        $sql = "SELECT $infields"
	       . " FROM cur, page"
	       . " WHERE page_namespace = $ns"
               . " AND "
	       . $self->_get_cmp_sql ("page_title",
				      $self->charset_encode ($name),
				      $args{ignore_case})
	       . " AND cur_namespace = page_namespace"
	       . " AND cur_title = page_title";
    }
    my @results = $self->charset_decode ($dbh->selectrow_array ($sql));
    return @results ? $results[0] : "" unless wantarray;
#    @results = ("", 0, "") unless @results;
    @data{@outfields} = @results;
    if ($args{version} || $ignore_case)
    {
	$data{name} = $self->__num_to_namespace ($data{ns}, $data{name});
    }
    else
    {
	$data{name} = $args{name};
    }
    $data{edit_type} = $data{edit_type} ? "Minor tidying" : "Normal edit"
	if defined $data{edit_type};
    $data{last_modified} = $self->_make_date ($data{last_modified});
    _make_metadata \%data unless $args{nometadata};
    return %data;
}



# $store->_retrieve_node_content (name    => $node_name,
#                                 version => $node_version);
# Params: 'name' is compulsory, 'version' is optional and defaults to latest.
# Returns a hash of data for C<retrieve_node> - content, version, last modified,
# or scalar, depending on context.
sub _retrieve_node_content
{
    return _retrieve_node_data @_, nometadata => 1;
}



=head2 list_all_nodes

Like the parent function, but accepts limit & offset arguments.

=cut

sub list_all_nodes
{
    my ($self, %args) = @_;
    my $dbh = $self->dbh;

    my $fields;
    if (wantarray)
    {
	$fields = "page_namespace, page_title";
    }
    else
    {
	$fields = "COUNT(*)";
    }

    my $sql = "SELECT $fields FROM page";
    my $limoffsql = _get_lim_off_sql (%args);
    $sql .= " " . $limoffsql if $limoffsql;

    my $nodes = $dbh->selectall_arrayref ($sql); 

    print STDERR "executing $sql\n"; # if $self->{debug};
    return $nodes->[0]->[0] unless wantarray;

    return map {
	$self->__num_to_namespace ($_->[0], $self->charset_decode ($_->[1]))
    } @$nodes;
}



=head2 list_recent_changes

Like the parent method, but the C<limit> argument may be used in conjunction
with the others (C<since>, C<days>, and C<between_days> are still mutually
exclusive).  A new, $args{between_secs} argument is also processed.  Its
contents should be two unix timestamps.

=cut

sub list_recent_changes
{
    my $self = shift;
    my %args = @_;

    my $exclusive = 0;
    foreach my $option (qw{days since between_days between_secs})
    {
	$exclusive++ if $args{$option};
    }
    croak "between_days, days, between_secs, & since options are "
	  . "mutually exclusive"
	if $exclusive > 1;

    $args{between_days} = [delete $args{days}, 0]
	if $args{days};

    if ($args{between_days})
    {
	croak "two arguments required for between_days"
	    unless @{$args{between_days}} == 2;

	my $now = gmtime;
	$args{between_secs} = [map {$now - $_ * ONE_DAY}
				   @{$args{between_days}}];
	delete $args{between_days};
    }

    $args{between_secs} = [delete $args{since}, gmtime]
	if $args{since};

    if ($args{between_secs})
    {
	croak "two arguments required for between_secs"
	    unless @{$args{between_secs}} == 2;
	$args{between_secs} = [map {scalar gmtime $_}
				   sort { $a <=> $b }
					@{$args{between_secs}}];
    }

    $args{limit} = delete $args{last_n_changes}
	if $args{last_n_changes};

    return $self->_find_recent_changes_by_criteria (%args);
}



sub _get_metadata_sql
{
    my ($self, $is, $table_prefix, $metadata, %args) = @_;
    my $sql;

    my $cmp;
    if ($is)
    {
	$cmp = "=";
    }
    else
    {
	$cmp = "!=";
    }

    foreach my $key (keys %$metadata)
    {
	if ($key eq "edit_type")
	{
	    if ($metadata->{$key} eq "Minor tidying")
	    {
		$sql .= " AND " . $table_prefix . "minor_edit $cmp 1"
	    }
	    elsif ($metadata->{$key} eq "Normal edit")
	    {
		$sql .= " AND " . $table_prefix . "minor_edit $cmp 0"
	    }
	    else
	    {
		confess "unrecognized edit_type: `" . $metadata->{$key} . "'";
	    }
	}
	elsif ($key eq "username")
	{
	    $sql .= " AND " . ($is ? "" : "NOT ")
		    . $self->_get_cmp_sql ($table_prefix . "user_text",
					   $self->charset_encode ($metadata->{$key}),
					   $args{ignore_case});
	}
	elsif ($key eq "patrolled")
	{
	    $sql .= " AND rc_patrolled $cmp " . $metadata->{$key};
	}
	else
	{
	    confess "unimplemented metadata key: `$key'";
	}
    }

    return $sql;
}



sub _get_lim_off_sql
{
    my (%args) = @_;

    if (exists $args{limit})
    {
	croak "Bad argument limit=`$args{limit}'"
	    unless defined $args{limit} && $args{limit} =~ /^\d+$/;
    }
    if (exists $args{offset})
    {
	croak "Bad argument offset=`$args{offset}'"
	    unless defined $args{offset} && $args{offset} =~ /^\d+$/;

	# This number is big.
	$args{limit} = 18446744073709551615 unless defined $args{limit};
    }

    return (defined $args{limit} ? "LIMIT $args{limit}" : "")
	   . ($args{offset} ? " OFFSET $args{offset}" : "");
}



sub _find_recent_changes_by_criteria
{
    my ($self, %args) = @_;
    my ($since, $between_days, $include_all_changes,
        $metadata_is,  $metadata_isnt, $metadata_was, $metadata_wasnt) =
         @args{qw(since between_days include_all_changes
                  metadata_is metadata_isnt metadata_was metadata_wasnt)};
    my $dbh = $self->dbh;
    my $sql;
    my $infields;
    my @outfields;
    my $ignore_case = exists $args{ignore_case}
		      ? $args{ignore_case} : $self->{ignore_case};

    my ($ns, $name) = $self->__namespace_to_num ($args{name})
	if $args{name};

    my ($tables, $table_prefix);

    # Don't know the rationale for this complex algorithm to determine which
    # table to use, but I copied it from Wiki::Toolkit::Store::Database.  It
    # works out such that, in order, include_all_changes == 1 will always force
    # the view including history.  metadata_is and metadata_isnt will always be
    # processed, history or no, but if either is set then metadata_was and
    # metadata_wasnt are ignored.  If neither metadata_is and metadata_isnt are
    # set, and either metadata_was or metadata_wasnt are set, then the view
    # including history is selected, regardless of the value of
    # include_all_changes.
    #
    # It seems to me like it would be easier to just accept two metadata
    # arguments and let include_all_changes switch tables, but I am
    # implementing this anyway for backwards compatibility.
    if ($include_all_changes || (!($metadata_is || $metadata_isnt)
				 && ($metadata_was || $metadata_wasnt)))
    {
	$include_all_changes = 1;
	$tables = "text LEFT JOIN recentchanges ON rc_this_oldid = old_id";
	$table_prefix = "old_";
	$metadata_is = $metadata_was unless $metadata_is;
	$metadata_isnt = $metadata_wasnt unless $metadata_isnt;
    }
    else
    {
	$tables = "cur INNER JOIN page ON page_namespace = cur_namespace"
		. " AND page_title = cur_title"
		. " LEFT JOIN recentchanges ON rc_this_oldid = page_latest";
	$table_prefix = "cur_";
    }


    if (wantarray)
    {
	if ($include_all_changes)
	{
	    $infields = "old_id, rc_new, ";
	}
	else
	{
	    $infields = "page_latest, cur_is_new, ";
	}

	$infields .= join ", ", map {$table_prefix . $_}
				    qw{user_text comment timestamp
				       minor_edit};
	@outfields = qw{version is_new username comment last_modified
			edit_type};

	$infields .= ", rc_patrolled";
	push @outfields, 'patrolled';

	unless ($args{name} && !$ignore_case)
	{
	    $infields .= ", " . join ", ", map {$table_prefix . $_}
					       qw{namespace title};
	    push @outfields, qw{ns name};
	}
    }
    else
    {
	$infields = "COUNT(*)";
    }

    $sql = "SELECT $infields"
	   . " FROM $tables";

    $sql .= " WHERE 1 = 1";

    $sql .= " AND " . $table_prefix . "namespace = $ns"
            . " AND "
	    . $self->_get_cmp_sql ($table_prefix . "title",
				   $self->charset_encode ($name),
				   $args{ignore_case})
	if $args{name};

    if ($args{between_secs})
    {
	# This function assumes that it was called via recent_changes, which
	# sorts the @{$args{between_secs}} array.
	my ($s, $f) = map {defined $_ ? ($_->strftime ($timestamp_fmt)) : $_}
			  @{$args{between_secs}};
	$sql .= " AND " . $table_prefix . "timestamp >= $s"
	     if $s;
	$sql .= " AND " . $table_prefix . "timestamp <= $f"
	     if $f;
    }

    $sql .= $self->_get_metadata_sql (1, $table_prefix, $metadata_is, %args)
	if $metadata_is;
    $sql .= $self->_get_metadata_sql (0, $table_prefix, $metadata_isnt, %args)
	if $metadata_isnt;

    $sql .= " ORDER BY " . $table_prefix . "timestamp DESC";

    my $limoffsql = _get_lim_off_sql (%args);
    $sql .= " " . $limoffsql if $limoffsql;

    print STDERR "executing $sql\n"; # if $self->{debug};
    my $nodes = $dbh->selectall_arrayref ($sql);

    return $nodes->[0]->[0] unless wantarray;

    my @newnodes;
    foreach my $i (0 .. (@$nodes - 1))
    {
	my %node;
	@node{@outfields} = @{$nodes->[$i]};
	if ($args{name} && !$ignore_case)
	{
	    $node{name} = $args{name};
	}
	else
	{
	    $node{name} =
		$self->__num_to_namespace ($node{ns},
					   $self->charset_decode ($node{name}));
	}
	$node{edit_type} = $node{edit_type} ? "Minor tidying" : "Normal edit";
	$node{last_modified} = $self->_make_date ($node{last_modified});
	_make_metadata \%node;
	push @newnodes, \%node;
    }
    return @newnodes;
}



# $self->_get_cmp_sql (FIELD, TEXT, IGNORE_CASE)
# Return text that would return TRUE in a DB query's WHERE clause, if
# the contents of FIELD matches TEXT, honoring first IGNORE_CASE, then
# defaulting to $self->{ignore_case} when IGNORE_CASE is undefined.
sub _get_cmp_sql
{
    my ($self, $field, $name, $ignore_case) = @_;
    $ignore_case = $self->{ignore_case} unless defined $ignore_case;
    my $dbh = $self->{_dbh};

    # The MySQL documentation says that unless one of the strings is declared
    # BINARY (e.g. "STRCMP ($field, BINARY $name)") then the string comparison
    # will be case insensitive, but for some reason that isn't working.  If it
    # was, then that would probably be faster than this:
    if ($ignore_case)
    {
	$field = "LOWER($field)";
	$name = lc $name;
    }

    return "NOT STRCMP($field, "
	   . $dbh->quote ($name) . ")";
}


# $store->_get_relative_version ($node_name, $node_version, $direction);
# Return the version number of the previous or next node, as specified.
sub _get_relative_version
{
    my ($self) = shift;

    my ($direction, $node, $version) = @_[0 .. 2];
    my %args = @_[3 .. $#_] if @_ > 3;

    my ($ns, $name) = $self->__namespace_to_num ($node);
    my $dbh = $self->dbh;
    my $sql = "SELECT old_id FROM text"
	      . " WHERE old_namespace = $ns"
	      . " AND "
	      . $self->_get_cmp_sql ("old_title",
				     $self->charset_encode ($name),
				     $args{ignore_case})
	      . " AND old_id $direction $version"
	      . " ORDER BY old_id";
    $sql .= " DESC" if $direction eq '<';
    $sql .= " LIMIT 1";

    print STDERR "executing $sql\n"; # if $self->{debug};
    my $ver = $dbh->selectrow_arrayref ($sql);
    return $ver->[0];
}



=head2 get_previous_version

    $store->get_previous_version ($node_name, $node_version, %other_args);

Given a version number, returns the previous version for the given node.
This function is necessary because mediawiki gives every revision of every
page a version number which is unique across all pages.

Techincally, node name shouldn't be necessary here, but it allows for a faster
search and you probably have it.  Not requiring it would be an easy hack.

=cut

sub get_previous_version
{
    my $self = shift;
    return $self->_get_relative_version ('<', @_);
}



=head2 get_next_version

    $store->get_next_version ($node_name, $node_version, %other_args);

Given a version number, returns the next version for the given node.
This function is necessary because mediawiki gives every revision of every
page a version number which is unique across all pages.

Techincally, node name shouldn't be necessary here, but it allows for a faster
search and you probably have it.  Not requiring it would be an easy hack.

=cut

sub get_next_version
{
    my $self = shift;
    return $self->_get_relative_version ('>', @_);
}



=head2 get_current_version

    $store->get_current_version ($node);
    $store->get_current_version (name => $node, %other_args);

Given a node, returns the current (most recent) version, or undef, if the node
does not exist.

=cut

sub get_current_version
{
    my $self = shift;
    my %args;

    if (@_ == 1)
    {
	$args{name} = $_[0];
    }
    else
    {
	%args = @_;
    }

    my ($ns, $name) = $self->__namespace_to_num ($args{name});
    my $dbh = $self->dbh;

    my $sql = "SELECT page_latest FROM page"
	      . " WHERE page_namespace = $ns"
	      . " AND "
	      . $self->_get_cmp_sql ("page_title",
				    $self->charset_encode ($name),
				    $args{ignore_case});
    print STDERR "executing $sql\n"; # if $self->{debug};
    my $ver = $dbh->selectrow_arrayref ($sql);
    return $ver ? $ver->[0] : undef;
}



sub _get_timestamp
{
    my $self = shift;
    # I don't care about no steenkin' timezones (yet).
    my $time = shift || localtime; # Overloaded by Time::Piece::Adaptive.
    # Make it into an object for strftime
    $time = localtime $time unless ref $time;
    return $time->strftime ($timestamp_fmt); # global
}



=head2 write_node_post_locking

Like the parent function, but works with the mediawiki DB.

=cut

sub write_node_post_locking
{
    my ($self, %args) = @_;
    my ($node, $content,
	$links_to_ref, $metadata) = @args{qw(node content links_to
					     metadata)};
    my $dbh = $self->dbh;

    croak "write_node_post_locking requires edit_type, and remote_ip metadata"
	unless $metadata && $metadata->{edit_type};

    my $timestamp = $self->_get_timestamp ();
    my @links_to = @{$links_to_ref || []}; # default to empty array

    my ($ns, $name) = $self->__namespace_to_num ($node);
    my $sql;

    my $userid;
    my $username;
    if ($metadata->{username})
    {
	$sql = "SELECT user_id, user_name FROM user"
	       . " WHERE "
	       . $self->_get_cmp_sql ("user_name",
				      $self->charset_encode ($metadata->{username}),
				      $args{ignore_case});
	print STDERR "executing $sql\n"; # if $self->{debug};
	my $rec = $dbh->selectrow_arrayref ($sql)
	    or croak "unable to retrieve user `$username': " . $dbh->errstr;
	$userid = $rec->[0];
	$username = $rec->[1];
    }
    else
    {
	$username = $metadata->{remote_ip};
	$userid = 0;
    }

    # First, remember the previous version number.
    my $old_old_id = $self->get_current_version ($node);

    # Always insert into the history table.
    $sql = "INSERT INTO "
	   . "text (old_namespace, old_title, old_text, old_comment, "
	   .       "old_user, old_user_text, old_timestamp, old_minor_edit, "
	   .       "old_flags, inverse_timestamp)"
	   . " VALUES ($ns, "
	   . $dbh->quote ($self->charset_encode ($name)) . ", "
	   . $dbh->quote ($self->charset_encode ($content)) . ", "
	   . $dbh->quote ($self->charset_encode ($metadata->{comment})) . ", "
	   . "$userid, "
	   . $dbh->quote ($username)
	   . ", "
	   . $dbh->quote ($timestamp) . ", "
	   . ($metadata->{edit_type} eq 'Minor tidying' ? "1" : "0")
	   . ", 'utf-8', " . (99999999999999 - $timestamp) . ")";
    print STDERR "executing $sql\n"; # if $self->{debug};
    $dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;
    my $new_old_id = $dbh->last_insert_id (undef, undef, undef, undef)
	or croak "Error retrieving last insert id: " . $dbh->errstr;

    # Either inserting a new page or updating an old one.
    my ($cur_id, $page_id);
    if ($old_old_id)
    {
	# update cur & page tables
	$sql = "SELECT cur_id FROM cur"
	       . " WHERE cur_namespace = $ns"
	       . " AND "
	       . $self->_get_cmp_sql ("cur_title",
				      $self->charset_encode ($name),
				      $args{ignore_case});
	print STDERR "executing $sql\n"; # if $self->{debug};
	$cur_id = $dbh->selectrow_arrayref ($sql)->[0]
	    or croak "Error retrieving cur id: " . $dbh->errstr;

        $sql = "UPDATE cur SET cur_text = "
	       . $dbh->quote ($self->charset_encode ($content)) . ", "
	       .              "cur_comment = "
	       . $dbh->quote ($self->charset_encode ($metadata->{comment}))
	       . ", "
	       .              "cur_user = $userid, "
	       .              "cur_user_text = "
	       . $dbh->quote ($username) . ", "
	       .              "cur_timestamp = " . $dbh->quote ($timestamp)
	       . ", "
	       .              "cur_is_redirect = 0, "
	       .              "cur_minor_edit = "
	       . ($metadata->{edit_type} eq 'Minor tidying' ? "1" : "0") . ", "
	       .              "cur_is_new = 0, "
	       .              "cur_touched = " . $dbh->quote ($timestamp)
	       . ", "
	       .              "inverse_timestamp = "
	       . $dbh->quote (99999999999999 - $timestamp)
	       . " WHERE cur_id = $cur_id";
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;

	$sql = "SELECT page_id FROM page"
	       . " WHERE page_namespace = $ns"
	       . " AND "
	       . $self->_get_cmp_sql ("page_title",
				      $self->charset_encode ($name),
				      $args{ignore_case});
	print STDERR "executing $sql\n"; # if $self->{debug};
	$page_id = $dbh->selectrow_arrayref ($sql)->[0]
	    or croak "Error retrieving page id: " . $dbh->errstr;

        $sql = "UPDATE page SET page_touched = " . $dbh->quote ($timestamp)
	       . ", "
	       .              "page_is_redirect = 0, "
	       .              "page_is_new = 0, "
	       .              "page_latest = $new_old_id, "
	       .              "page_len = "
	       . length ($self->charset_encode ($content))
	       . " WHERE page_id = $page_id";
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;
    }
    else
    {
        $sql = "INSERT INTO cur (cur_namespace, cur_title, cur_text, "
	       .                "cur_comment, cur_user, cur_user_text, "
	       .                "cur_timestamp, cur_counter, cur_is_redirect, "
	       .                "cur_is_new, cur_random, cur_minor_edit, "
	       .                "cur_touched, inverse_timestamp)"
	       . " VALUES ($ns, "
	       . $dbh->quote ($self->charset_encode ($name)) . ", "
	       . $dbh->quote ($self->charset_encode ($content)) . ", "
	       . $dbh->quote ($self->charset_encode ($metadata->{comment}))
	       . ", $userid, "
	       . $dbh->quote ($username) . ", "
	       . $dbh->quote ($timestamp)
	       . ", 0, 0, 1, 0, "
	       . ($metadata->{edit_type} eq 'Minor tidying' ? "1" : "0") . ", "
	       . $dbh->quote ($timestamp) . ", "
	       . $dbh->quote (99999999999999 - $timestamp) . ")";
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;

	$cur_id = $dbh->last_insert_id (undef, undef, undef, undef)
	    or croak "Error retrieving last insert id: " . $dbh->errstr;

        $sql = "INSERT INTO page (page_namespace, page_title, page_touched, "
	       .                 "page_counter, page_is_redirect, "
	       .                 "page_is_new, page_random, page_latest, "
	       .                 "page_len)"
	       . " VALUES ($ns, "
	       . $dbh->quote ($self->charset_encode ($name)) . ", "
	       . $dbh->quote ($timestamp)
	       . ", 0, 0, 1, 0, $new_old_id, "
	       . length ($self->charset_encode ($content)) . ")";
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;

	$page_id = $dbh->last_insert_id (undef, undef, undef, undef)
	    or croak "Error retrieving last insert id: " . $dbh->errstr;

	# Fix broken links that are repaired by this insert.
	$sql = "SELECT bl_from FROM brokenlinks"
	     . " WHERE "
	     . $self->_get_cmp_sql ("bl_to", $self->charset_encode ($node),
				    $args{ignore_case});
	print STDERR "executing $sql\n"; # if $self->{debug};
	my $pbls = $dbh->selectall_arrayref ($sql)
	    or croak $dbh->errstr;
	if (@$pbls)
	{
	    $sql = "DELETE FROM brokenlinks"
		   . " WHERE "
		   . $self->_get_cmp_sql ("bl_to",
					  $self->charset_encode ($node),
					  $args{ignore_case});
	    print STDERR "executing $sql\n"; # if $self->{debug};
	    $dbh->do ($sql)
		or croak $dbh->errstr;

	    # Assuming this is already in pagelinks.
	    $sql = "INSERT INTO links (l_from, l_to) VALUES (?, $page_id)";
	    my $sth = $dbh->prepare ($sql) or croak $dbh->errstr;
	    foreach (@$pbls)
	    {
		$sth->execute ($_) or croak $dbh->errstr;
	    }
	    $sth->finish;
	}
    }

    # Always insert into the recent changes table.
    $sql = "INSERT INTO "
	   . "recentchanges (rc_timestamp, rc_cur_time, rc_user, "
	   .                "rc_user_text, rc_namespace, rc_title, "
	   .                "rc_comment, rc_minor, rc_bot, rc_new, "
	   .		    "rc_cur_id, rc_this_oldid, rc_last_oldid, "
	   .                "rc_type, rc_moved_to_ns, rc_patrolled, rc_ip)"
	   . " VALUES ("
	   . $dbh->quote ($timestamp) . ", "
	   . $dbh->quote ($timestamp)
	   . ", $userid, "
	   . $dbh->quote ($username)
	   . ", $ns, "
	   . $dbh->quote ($self->charset_encode ($name)) . ", "
	   . $dbh->quote ($self->charset_encode ($metadata->{comment})) . ", "
	   . ($metadata->{edit_type} eq 'Minor tidying' ? 1 : 0)
	   . ", 0, "
	   . (defined $old_old_id ? 0 : 1)
	   . ", $cur_id, $new_old_id, "
	   . (defined $old_old_id ? $old_old_id : 0)
	   . ", 0, $ns, 0, "
	   . $dbh->quote ($metadata->{remote_ip})
	   . ")";
    print STDERR "executing $sql\n"; # if $self->{debug};
    $dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;

    # Add to backlinks if possible
    $dbh->do ("DELETE FROM links "
	      . " WHERE l_from = $page_id")
	or croak $dbh->errstr;
    $dbh->do ("DELETE FROM brokenlinks "
	      . " WHERE bl_from = $page_id")
	or croak $dbh->errstr;
    $dbh->do ("DELETE FROM pagelinks "
	      . " WHERE pl_from = $page_id")
	or croak $dbh->errstr;

    my $lastlink;
    my @locallinks;
    foreach (@links_to)
    {
	next if $lastlink && $_ eq $lastlink;
	$lastlink = $_;
	push @locallinks, $_ unless m(^\w+://) || /^\s*$/;
    }

    if (@locallinks)
    {
	my ($s, $n);
	$sql = "INSERT INTO links (l_from, l_to)"
	       . " SELECT $page_id, page_id FROM page"
	       . " WHERE "
	       . join (" OR ",
		       map {($s, $n) = $self->__namespace_to_num ($_);
			    "(page_namespace = $s AND "
			    . $self->_get_cmp_sql ("page_title",
						   $self->charset_encode ($n),
						   $args{ignore_case}) . ")"}
		           @locallinks);
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak $dbh->errstr;
    }

    # Insert into the pagelinks table, which records everything, and the
    # brokenlinks table, which records linked pages that don't exist.
    $sql = "INSERT INTO pagelinks (pl_from, pl_namespace, pl_title)"
	   . " VALUES ($page_id, ?, ?)";
    my $st1 = $dbh->prepare ($sql) or croak $dbh->errstr;
    $sql = "SELECT page_id FROM page"
	   . " WHERE page_namespace = ? AND page_title = ?";
    my $st2 = $dbh->prepare ($sql) or croak $dbh->errstr;
    $sql = "INSERT INTO brokenlinks (bl_from, bl_to)"
	   . " VALUES ($page_id, ?)";
    my $st3 = $dbh->prepare ($sql) or croak $dbh->errstr;
    foreach my $link (sort @locallinks)
    {
	my $en = ($self->charset_encode ($link))[0];
	my ($s, $n) = $self->__namespace_to_num ($en);

	$st1->execute ($s, $n);
	$st2->execute ($s, $n);
	my ($tid) = $st2->fetchrow_array;
	$st3->execute ($en) unless $tid;
    }
    $st1->finish;
    $st2->finish;
    $st3->finish;

    # And also store any metadata.  Note that any entries already in the
    # metadata table refer to old versions, so we don't need to delete them.
    foreach my $type (keys %$metadata)
    {
	croak "unknown metadata key `$type'"
	    unless grep qr/^\Q$type\E$/, (qw{comment edit_type formatter
					     username remote_ip});
    }

    # Finally call post_write on any plugins.
    my @plugins = @{$args{plugins} || [ ]};
    foreach my $plugin (@plugins) {
        if ($plugin->can ("post_write"))
	{
            $plugin->post_write (node     => $node,
				 version  => $new_old_id,
				 content  => $content,
				 metadata => $metadata);
	}
    }

    return 1;
}



=head2 node_exists

  $store->node_exists ($node);
  $store->node_exists (name => $node, %other_args);

Like the parent function of the same name, but much faster.  Really just
a wrapper for get_current_version, returns the current version number when
it exists and undef otherwise.

=cut

sub node_exists
{
    my $self = shift;
    return $self->get_current_version (@_);
}



=head2 list_backlinks

  # List all nodes that link to the Home Page.
  my @links = $store->list_backlinks (node => "Home Page");

=cut

sub list_backlinks
{
    my ($self, %args) = @_;
    my $node = $args{node};
    croak "Must supply a node name" unless $node;

    my ($ns, $name) = $self->__namespace_to_num ($node);
    my $dbh = $self->dbh;

    my $fields = "DISTINCT page_namespace, page_title";
    $fields = "COUNT($fields)" unless wantarray;

    my $sql = "SELECT $fields"
	      . " FROM page p, pagelinks pl"
	      . " WHERE pl_namespace = $ns"
	      . " AND "
	      . $self->_get_cmp_sql ("pl_title",
				     $self->charset_encode ($name),
				     $args{ignore_case})
	      . " AND page_id = pl_from";

    my $limoffsql = _get_lim_off_sql (%args);
    $sql .= " " . $limoffsql if $limoffsql;

    print STDERR "executing $sql\n"; # if $self->{debug};
    my $sth = $dbh->prepare ($sql);
    $sth->execute or croak $dbh->errstr;

    return ($sth->fetchrow_array)[0] unless wantarray;

    my @backlinks;
    while (my ($ns_from, $from) = $sth->fetchrow_array)
    {
	push @backlinks,
	     $self->__num_to_namespace ($ns_from,
					$self->charset_decode ($from));
    }
    return @backlinks;
}



=head2 list_dangling_links

  # List all nodes that have been linked to from other nodes but don't
  # yet exist.
  my @links = $store->list_dangling_links;

Each node is returned once only, regardless of how many other nodes
link to it.  Nodes are be returned unsorted.

=cut

sub list_dangling_links
{
    my $self = shift;
    my $dbh = $self->dbh;
    my $sql = "SELECT DISTINCT bl_to FROM brokenlinks";
    my $sth = $dbh->prepare ($sql);
    print STDERR "executing $sql\n"; # if $self->{debug};
    $sth->execute or croak $dbh->errstr;
    my @links;
    while (my ($link) = $self->charset_decode ($sth->fetchrow_array))
    {
        push @links, $link;
    }
    return @links;
}



=head2 list_dangling_links_w_count

  # List all nodes that have been linked to from other nodes but don't
  # yet exist, with a reference count.
  foreach my $link ($store->list_dangling_links_w_count)
  {
    print "Missing `", $link->[0], "' has ", $link->[1], " references.\n";
  }

Nodes are returned sorted primarily by the reference count, greatest first, and
secondarily in alphabetical order.

=cut

sub list_dangling_links_w_count
{
    my ($self, %args) = @_;
    my $dbh = $self->dbh;
    my ($fields, $tail);

    if (wantarray)
    {
	$fields = "bl_to, COUNT(*)";
	$tail = "GROUP BY bl_to ORDER BY COUNT(*) DESC, bl_to";
    }
    else
    {
	$fields = "COUNT(DISTINCT bl_to)";
    }

    my $limoffsql = _get_lim_off_sql (%args);
    $tail .= ($tail ? " " : "") . $limoffsql if $limoffsql;

    my $sql = "SELECT $fields FROM brokenlinks";
    $sql .= " " . $tail if $tail;

    print STDERR "executing $sql\n"; # if $self->{debug};
    my $sth = $dbh->prepare ($sql);
    $sth->execute or croak $dbh->errstr;

    return ($sth->fetchrow_array)[0] unless wantarray;

    my @links;
    while (my @row = $sth->fetchrow_array)
    {
        push @links, [$self->charset_decode ($row[0]), $row[1]];
    }
    return @links;
}



=head2 get_user_info

  my ($username, $email_validated)
	= $store->get_user_info (name => $username,
				 password => $password,
				 fields => [name, email_authenticated,
					    token],
				 %other_args);

Given a user name, return the requested fields if the user exists and undef,
otherwise.  Given a password or a token, undef is also returned if the
specified password or token is incorrect.

The list of fields to return defaults to C<name>, C<email_authenticated>,
& C<token>.

The returned user name may be different from the one passed in when
$args{ignore_case} is set.

=cut

sub get_user_info
{
    my ($self, %args) = @_;
    my $dbh = $self->{_dbh};

    my ($where, $count);
    for my $key (qw{name id email})
    {
	if ($args{$key})
	{
	    $count++;
	    $where = $args{id}
		     ? "user_id = " . $args{id}
		     : $self->_get_cmp_sql ("user_$key",
					    $self->charset_encode ($args{$key}),
					    $args{email}
					    ? 1 : $args{ignore_case});
	}
    }
    croak "Must supply one and only one of `name', `id', or `email'"
	unless $count == 1;

    $count = 0;
    for my $key (qw{password token email_token})
    {
	$count++ if $args{$key};
    }
    croak "Must supply only one of `password', `token', or `email_token'"
	if $count > 1;

    my @fields = map {"user_$_"}
		     ($args{fields} ? @{$args{fields}}
				    : qw(name email_authenticated token));

    if ($args{password})
    {
	push @fields, qw(user_id user_password);
    }
    elsif ($args{token})
    {
	push @fields, qw(user_token);
    }
    elsif ($args{email_token})
    {
	push @fields, qw(user_id user_email_token user_email_token_expires);
    }

    my $sql = "SELECT " . join (", ", @fields)
	      . " FROM user"
	      . " WHERE $where";

    print STDERR "executing $sql\n"; # if $self->{debug};
    my $userinfo = $dbh->selectall_arrayref ($sql)
	or croak "Error retrieving user info: " . $dbh->errstr;

    # Check that one and only one user was found.
    return undef unless @$userinfo;  # failed login
    die "multiple users found matching `$args{name}'"
	unless @$userinfo == 1;      # Corrupt database.
    $userinfo = $userinfo->[0];

    if ($args{password})
    {
	# Check the password.
	my ($uid, $password);
	$password = pop @$userinfo;
	$uid = pop @$userinfo;

	my $ep = md5_hex ($uid . "-" . md5_hex ($args{password}));
	return undef unless $ep eq $password;
    }
    elsif ($args{token})
    {
	# Check the token.
	my $token = pop @$userinfo;
	return undef unless $args{token} eq $token;
    }
    elsif ($args{email_token})
    {
	# Check the token.
	my ($uid, $expires, $token);
	$expires = $self->_make_date (pop @$userinfo);
	$token = pop @$userinfo;
	$uid = pop @$userinfo;
	my $now = gmtime;

	return undef unless $args{email_token} eq $token
			    && $now < $expires;

	$self->update_user (id => $uid, email_authenticated => $now);
    }

    # The remaining fields were requested.
    for (my $i = 0; $i < @fields; $i++)
    {
      $userinfo->[$i] = $self->_make_date ($userinfo->[$i])
	if defined $userinfo->[$i] && $fields[$i] =~ /_(?:touched|expires)$/;
    }
    return @$userinfo;
}



=head2 create_new_user

  my @errmsgs = $store->create_new_user (name => $username, password => $p);

Create a new user.  C<name> and C<password> are required arguments.
Optional arguments are C<email> & C<real_name>.

Returns a potentially empty list of error messages.

=cut

# Internal function to create and update users.
#
# This function makes some assumptions enforced by its callers.  Don't use it
# directly.
sub _update_user
{
    my ($self, %args) = @_;

    my $dbh = $self->{_dbh};

    # Fields to update/insert.
    my (@fields, @values);

    # For the timestamp, and perhaps email_token_expires.
    my $now = gmtime;
    $args{touched} = $now;

    $args{email_token_expires} = $now + $args{email_token_expires}
      if exists $args{email_token_expires}
	 && !(ref $args{email_token_expires}
	      && $args{email_token_expires}->isa ('Time::Piece'));

    my @infields = qw(real_name email email_token email_token_expires
		      email_authenticated token touched password);
    push @infields, "name" if $args{create};
    for my $field (@infields)
    {
	if (exists $args{$field})
	{
	    push @fields, "user_$field";
	    $args{$field}->set_stringify ($timestamp_fmt)
		if ref $args{$field}
		   && $args{$field}->isa ('Time::Piece::Adaptive');
	    push @values, $dbh->quote ($self->charset_encode ($args{$field}));
	}
    }

    # touched and name are always included.
    croak "Must include at least one field for update"
	unless @fields > ($args{create} ? 2 : 1);

    my $uid;
    my $sql;
    if ($args{create})
    {
	$sql = "INSERT INTO user (" . join (", ", @fields)
	       . ") VALUES (" . join (", ", @values) . ")";
    }
    else
    {
	my %qa;
	if ($args{id})
	{
	    $qa{id} = $args{id};
	}
	else
	{
	    $qa{name} = $args{name};
	}
	($uid) = $self->get_user_info (%qa, fields => ["id"]);

	# Check that one and only one existing user was found.
	return "No such user, `" . $args{name} . "'."
	    unless $uid;

	$sql = "UPDATE user SET "
	       . join (", ", map ({"$fields[$_] = $values[$_]"} (0..$#fields)))
	       . " WHERE "
	       . "user_id = " . $uid;
    }

    print STDERR "executing $sql\n"; # if $self->{debug};
    $dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;

    if ($args{create})
    {
	# Get the new user ID and update the password.
	$uid = $dbh->last_insert_id (undef, undef, undef, undef)
	    or croak "Error retrieving last insert id: " . $dbh->errstr;
    }

    if ($args{password})
    {
	# Encode the password.
	my $ep = md5_hex ($uid . "-" . md5_hex ($args{password}));

	# Update the password.
	$sql = "UPDATE user SET user_password = " . $dbh->quote ($ep)
	       . " WHERE user_id = $uid";
	print STDERR "executing $sql\n"; # if $self->{debug};
	$dbh->do ($sql) or croak "Error updating database: " . $dbh->errstr;
    }

    return;
}

sub create_new_user
{
    my ($self, %args) = @_;

    croak "name is a required argument" unless $args{name};
    croak "password is a required argument" unless $args{password};

    my $dbh = $self->{_dbh};

    # Verify that the user does not exist.
    my $sql = "SELECT user_name FROM user"
	       . " WHERE "
	       . $self->_get_cmp_sql ("user_name",
				      $self->charset_encode ($args{name}),
				      $args{ignore_case});
    print STDERR "executing $sql\n"; # if $self->{debug};
    my $userinfo = $dbh->selectall_arrayref ($sql)
	or croak "Error retrieving user info: " . $dbh->errstr;

    # Check no existing user was found.
    return "User `" . $userinfo->[0]->[0] . "' already exists."
	if @$userinfo;

    return $self->_update_user (%args, create => 1);
}



=head2 update_user

Like C<create_user>, except only either C<name> or C<id>, and one field to
update, are required arguments.

=cut

sub update_user
{
    my ($self, %args) = @_;

    croak "One, and only one, of `name' and `id', are required arguments."
	unless !($args{name} && $args{id}) && ($args{name} || $args{id});

    return $self->_update_user (%args);
}



=head2 schema_current

Overrides the parent function of the same name.  At the moment it only returns
(0, 0).

=cut

sub schema_current
{
    return (0, 0);
}

=head2 get_interwiki_url

  $url = $store->get_interwiki_url ($wikilink);

Converts an interwiki link (like C<Wikipedia:Perl>) to a URL (in this example,
something like C<http://en.wikipedia.org/wiki/Perl>), or returns undef if
C<$wikilink> does not appear to refer to a known wiki.  This match is always
case insensitive because users are often careless.

=cut

# Hrm.  It seems silly to make these errors fatal.  Perhaps it should be a
# configuration option.
sub get_interwiki_url
{
    my ($self, $wl) = @_;
    my $dbh = $self->{_dbh};

    my ($prefix, $suffix) = ($wl =~ /^([^:]*):+([^:].*)$/);
    return unless $prefix;

    my $sql = "SELECT iw_url FROM interwiki"
	       . " WHERE "
	       . $self->_get_cmp_sql ("iw_prefix",
				      $self->charset_encode ($prefix), 1);
    print STDERR "executing $sql\n"; # if $self->{debug};
    my $rows = $dbh->selectall_arrayref ($sql)
	or croak "Error retrieving interwiki info: " . $dbh->errstr;

    print STDERR "Multiple interwiki entries found for `$prefix'."
	if @$rows > 1;
    return unless @$rows == 1;

    my $url = $rows->[0][0];
    $url =~ s/\$1/$suffix/;
    return $url;
}



=head1 SEE ALSO

=over 4

=item L<Wiki::Toolkit::Kwiki>

=item L<Wiki::Toolkit::Formatter::Mediawiki>

=item L<Wiki::Toolkit>

=item L<Wiki::Toolkit::Store::Database>

=item L<Wiki::Toolkit::Store::MySQL>

=item L<Time::Piece::Adaptive>

=back

=head1 AUTHOR

Derek Price, C<< <derek at ximbiot.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-cgi-wiki-store-mediawiki at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Wiki-Toolkit-Store-Mediawiki>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Wiki::Toolkit::Store::Mediawiki

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Wiki-Toolkit-Store-Mediawiki>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Wiki-Toolkit-Store-Mediawiki>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Wiki-Toolkit-Store-Mediawiki>

=item * Search CPAN

L<http://search.cpan.org/dist/Wiki-Toolkit-Store-Mediawiki>

=back

=head1 ACKNOWLEDGEMENTS

My thanks go to Kake Pugh, for providing the well written L<Wiki::Toolkit> and
L<Wiki::Toolkit::Kwiki> modules, which got me started on this.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Derek Price, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Wiki::Toolkit::Store::Mediawiki
