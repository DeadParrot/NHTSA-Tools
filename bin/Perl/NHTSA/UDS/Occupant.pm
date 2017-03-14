package NHTSA::UDS::Occupant;

require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::Occupant.pm
#    PURPOSE: To encapsulate the data for the Occupant
#	      information associated with a UDS-1992 signal file
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

sub setOccTyp {
	my ($self,$szOcctyp) = @_;
	
	$szOcctyp = $self->_upcase_string($szOcctyp);
	
	if ( defined($szOcctyp) &&
	     ( length($szOcctyp) > 0 ) )
	{
		$self->{'_occtyp'} = $szOcctyp;
	}
	else
	{
		$self->{'_occtyp'} = "";
	}
}
sub setOccAge {
	my ($self,$nOccage) = @_;
	
	if ( defined($nOccage) &&
	     ( $nOccage >= 0 ) )
	{
		$self->{'_occage'} = $nOccage;
	}
	else
	{
		$self->{'_occage'} = 0;
	}
}
sub setOccSex {
	my ($self,$szOccsex) = @_;
	
	$szOccsex = $self->_upcase_string($szOccsex);
	
	if ( defined($szOccsex) &&
	     ( length($szOccsex) > 0 ) )
	{
		$self->{'_occsex'} = $szOccsex;
	}
	else
	{
		$self->{'_occsex'} = "";
	}
}
sub setOccWt {
	my ($self,$fOccwt) = @_;
	
	if ( defined($fOccwt) &&
	     ( $fOccwt >= 0.0 ) )
	{
		$self->{'_occwt'} = $fOccwt;
	}
	else
	{
		$self->{'_occwt'} = 0.0;
	}
}
sub setDumSiz {
	my ($self,$szDumsiz) = @_;
	
	$szDumsiz = $self->_upcase_string($szDumsiz);
	
	if ( defined($szDumsiz) &&
	     ( length($szDumsiz) > 0 ) )
	{
		$self->{'_dumsiz'} = $szDumsiz;
	}
	else
	{
		$self->{'_dumsiz'} = "";
	}
}
sub setRestr1 {
	my ($self,$szRestr1) = @_;
	
	$szRestr1 = $self->_upcase_string($szRestr1);
	
	if ( defined($szRestr1) &&
	     ( length($szRestr1) > 0 ) )
	{
		$self->{'_restr1'} = $szRestr1;
	}
	else
	{
		$self->{'_restr1'} = "";
	}
}
sub setRestr2 {
	my ($self,$szRestr2) = @_;
	
	$szRestr2 = $self->_upcase_string($szRestr2);
	
	if ( defined($szRestr2) &&
	     ( length($szRestr2) > 0 ) )
	{
		$self->{'_restr2'} = $szRestr2;
	}
	else
	{
		$self->{'_restr2'} = "";
	}
}
sub getOccTyp {
	my $self = shift;
	
	return($self->{'_occtyp'});
}
sub getOccAge {
	my $self = shift;
	
	return($self->{'_occage'});
}
sub getOccSex {
	my $self = shift;
	
	return($self->{'_occsex'});
}
sub getOccWt {
	my $self = shift;
	
	return($self->{'_occwt'});
}
sub getDumSiz {
	my $self = shift;
	
	return($self->{'_dumsiz'});
}
sub getRestr1 {
	my $self = shift;
	
	return($self->{'_restr1'});
}
sub getRestr2 {
	my $self = shift;
	
	return($self->{'_restr2'});
}

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

########################################################################
#
#	End of Package
#
########################################################################
1;
