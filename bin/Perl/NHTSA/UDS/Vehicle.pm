package NHTSA::UDS::Vehicle;

require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::Vehicle.pm
#    PURPOSE: To encapsulate the data for the Vehicle / Component
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

	$self->setCmpNo(0);
	$self->setCmpTyp(undef);
	$self->setCmpDsc(undef);
	$self->setMake(undef);
	$self->setModel(undef);
	$self->setYear(undef);
	$self->setBody(undef);
	$self->setEngine(undef);
	$self->setCmpWt(0.0);
	$self->setCmpSpd(0.0);
	
}

########################################################################
#
#	Accessors
#
########################################################################

sub setCmpNo {
	my ($self,$nCmpno) = @_;
	
	if ( defined($nCmpno) &&
	     ( $nCmpno >= 0 ) )
	{
		$self->{'_cmpno'} = $nCmpno;
	}
	else
	{
		$self->{'_cmpno'} = 0;
	}
}
sub setCmpTyp {
	my ($self,$szCmptyp) = @_;
	
	$szCmptyp = $self->_upcase_string($szCmptyp);
	
	if ( defined($szCmptyp) &&
	     ( length($szCmptyp) > 0 ) )
	{
		$self->{'_cmptyp'} = $szCmptyp;
	}
	else
	{
		$self->{'_cmptyp'} = "";
	}
}
sub setCmpDsc {
	my ($self,$szCmpdsc) = @_;
	
	$szCmpdsc = $self->_upcase_string($szCmpdsc);
	
	if ( defined($szCmpdsc) &&
	     ( length($szCmpdsc) > 0 ) )
	{
		$self->{'_cmpdsc'} = $szCmpdsc;
	}
	else
	{
		$self->{'_cmpdsc'} = "";
	}
}
sub setMake {
	my ($self,$szMake) = @_;
	
	$szMake = $self->_upcase_string($szMake);
	
	if ( defined($szMake) &&
	     ( length($szMake) > 0 ) )
	{
		$self->{'_make'} = $szMake;
	}
	else
	{
		$self->{'_make'} = "";
	}
}
sub setModel {
	my ($self,$szModel) = @_;
	
	$szModel = $self->_upcase_string($szModel);
	
	if ( defined($szModel) &&
	     ( length($szModel) > 0 ) )
	{
		$self->{'_model'} = $szModel;
	}
	else
	{
		$self->{'_model'} = "";
	}
}
sub setYear {
	my ($self,$nYear) = @_;
	my @aTimeDate;
	
	if ( defined($nYear) &&
	     ( $nYear > 0 ) )
	{
		$self->{'_year'} = $nYear;
	}
	else
	{
		@aTimeDate = localtime(time());
		$self->{'_year'} = sprintf("%d",($aTimeDate[6]+1900));
	}
}
sub setBody {
	my ($self,$szBody) = @_;
	
	$szBody = $self->_upcase_string($szBody);
	
	if ( defined($szBody) &&
	     ( length($szBody) > 0 ) )
	{
		$self->{'_body'} = $szBody;
	}
	else
	{
		$self->{'_body'} = "";
	}
}
sub setEngine {
	my ($self,$szEngine) = @_;
	
	$szEngine = $self->_upcase_string($szEngine);
	
	if ( defined($szEngine) &&
	     ( length($szEngine) > 0 ) )
	{
		$self->{'_engine'} = $szEngine;
	}
	else
	{
		$self->{'_engine'} = "";
	}
}
sub setCmpWt {
	my ($self,$fCmpwt) = @_;
	
	if ( defined($fCmpwt) &&
	     ( $fCmpwt >= 0.0 ) )
	{
		$self->{'_cmpwt'} = $fCmpwt;
	}
	else
	{
		$self->{'_cmpwt'} = 0.0;
	}
}
sub setCmpSpd {
	my ($self,$fCmpspd) = @_;
	
	if ( defined($fCmpspd) )
	{
		$self->{'_cmpspd'} = $fCmpspd;
	}
	else
	{
		$self->{'_cmpspd'} = 0.0;
	}
}

sub getCmpNo {
	my $self = shift;
	
	return($self->{'_cmpno'});
}
sub getCmpTyp {
	my $self = shift;
	
	return($self->{'_cmptyp'});
}
sub getCmpDsc {
	my $self = shift;
	
	return($self->{'_cmpdsc'});
}
sub getMake {
	my $self = shift;
	
	return($self->{'_make'});
}
sub getModel {
	my $self = shift;
	
	return($self->{'_model'});
}
sub getYear {
	my $self = shift;
	
	return($self->{'_year'});
}
sub getBody {
	my $self = shift;
	
	return($self->{'_body'});
}
sub getEngine {
	my $self = shift;
	
	return($self->{'_engine'});
}
sub getCmpWt {
	my $self = shift;
	
	return($self->{'_cmpwt'});
}
sub getCmpSpd {
	my $self = shift;
	
	return($self->{'_cmpspd'});
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
