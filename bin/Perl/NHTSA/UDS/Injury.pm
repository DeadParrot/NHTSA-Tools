package NHTSA::UDS::Injury;

require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::Injury.pm
#    PURPOSE: To encapsulate the data for the Injury
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

	$self->setHIC(0.0);
	$self->setT1(0.0);
	$self->setT2(0.0);
	$self->setHICDTUP(0.0);
	$self->setClip3M(0.0);
	$self->setCSI(0.0);
	$self->setAIS(undef);
}

########################################################################
#
#	Accessors
#
########################################################################

sub setHIC {
	my ($self,$fHic) = @_;
	
	if ( defined($fHic) &&
	     ( $fHic >= 0.0 ) )
	{
		$self->{'_hic'} = $fHic;
	}
	else
	{
		$self->{'_hic'} = 0.0;
	}
}
sub setT1 {
	my ($self,$fT1) = @_;
	
	if ( defined($fT1) )
	{
		$self->{'_t1'} = $fT1;
	}
	else
	{
		$self->{'_t1'} = 0.0;
	}
}
sub setT2 {
	my ($self,$fT2) = @_;
	
	if ( defined($fT2) )
	{
		$self->{'_t2'} = $fT2;
	}
	else
	{
		$self->{'_t2'} = 0.0;
	}
}
sub setHICDTUP {
	my ($self,$fHicdtup) = @_;
	
	if ( defined($fHicdtup) )
	{
		$self->{'_hicdtup'} = $fHicdtup;
	}
	else
	{
		$self->{'_hicdtup'} = 0.0;
	}
}
sub setClip3M {
	my ($self,$fClip3m) = @_;
	
	if ( defined($fClip3m) )
	{
		$self->{'_clip3m'} = $fClip3m;
	}
	else
	{
		$self->{'_clip3m'} = 0.0;
	}
}
sub setCSI {
	my ($self,$fCsi) = @_;
	
	if ( defined($fCsi) )
	{
		$self->{'_csi'} = $fCsi;
	}
	else
	{
		$self->{'_csi'} = 0.0;
	}
}
sub setAIS {
	my ($self,$szAis) = @_;
	
	$szAis = $self->_upcase_string($szAis);
	
	if ( defined($szAis) &&
	     ( length($szAis) >0 ) &&
	     ( $szAis =~ /^[0-9]$/ ) )
	{
		$self->{'_ais'} = $szAis;
	}
	else
	{
		$self->{'_ais'} = "";
	}
}
sub getHIC {
	my $self = shift;
	
	return($self->{'_hic'});
}
sub getT1 {
	my $self = shift;
	
	return($self->{'_t1'});
}
sub getT2 {
	my $self = shift;
	
	return($self->{'_t2'});
}
sub getHICDTUP {
	my $self = shift;
	
	return($self->{'_hicdtup'});
}
sub getClip3M {
	my $self = shift;
	
	return($self->{'_clip3m'});
}
sub getCSI {
	my $self = shift;
	
	return($self->{'_csi'});
}
sub getAIS {
	my $self = shift;
	
	return($self->{'_ais'});
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
