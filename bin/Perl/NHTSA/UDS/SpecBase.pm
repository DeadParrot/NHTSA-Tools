package NHTSA::UDS::SpecBase;

use strict;

########################################################################
#      CLASS: NHTSA::UDS::SpecBase
#    PURPOSE: To encapsulate common methods / operations for
#	      data in UDS-1992 signal files
#     AUTHOR: Andrew Orndorff (andrew.orndorff@nhtsa.dot.gov)
#     SOURCE:
#    CREATED: 1999/03/24
########################################################################


########################################################################
#
#	Package Constants
#
########################################################################


########################################################################
#
#	Constructor
#
########################################################################

sub new {
	my($class,@param) = @_;
	
	my $self = {};
	
	bless $self,ref($class) || $class;
	
	$self->_initialize(@param);
	
	$self;
}

sub _initialize {
	my $self = shift;

}

########################################################################
#
#	Accessors
#
########################################################################

########################################################################
#
#	Public Instance Methods
#
########################################################################

########################################################################
#
#	Private Instance Methods
#
########################################################################
#
# Trims leading/trailing whitespace, compresses whitespace, upcases a string
#
sub _upcase_string {
	my ($self,$szString) = @_;
	
	if ( defined($szString) )
	{
		$szString =~ s/(^\s+|\s+$)//g;
		$szString =~ s/\s+/ /g;
#		$szString =~ tr/a-z/A-Z/; ############### SGM: Allow mixed case for some fields
		
		return($szString);
	}
	else
	{
		return(undef);
	}
}

########################################################################
#
#	End of Package
#
########################################################################
1;
