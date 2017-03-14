package NHTSA::UDS::Test;

require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::Test.pm
#    PURPOSE: To encapsulate the data for the Test information 
#	      associated with a UDS-1992 signal file
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

	$self->setTstSrc("USER DATA");
	$self->setTstNo(0);
	$self->setTstNam(undef);
	$self->setTitle(undef);
	$self->setTstPrf("OTHER");
	$self->setTstRef(undef);
	$self->setTstCfn("OTH");
	$self->setImpAng(0);
	$self->setClsSpd(0.0);
}

########################################################################
#
#	Accessors
#
########################################################################

sub setTstSrc {
	my ($self,$szTstsrc) = @_;
	
	$szTstsrc = $self->_upcase_string($szTstsrc);
	
	if ( defined($szTstsrc) &&
	     ( length($szTstsrc) > 0 ) )
	{
		$self->{'_tstsrc'} = $szTstsrc;
	}
	else
	{	
		$self->{'_tstsrc'} = "";
	}
}
sub setTstNo {
	my ($self,$nTstno) = @_;
	
	if ( defined($nTstno) &&
	     ( $nTstno >= 0 ) )
	{
		$self->{'_tstno'} = $nTstno;
	}
	else
	{	
		$self->{'_tstno'} = 0;
	}
	
	if ( !defined($self->getTstNam()) )
	{
		$self->setTstNam(sprintf("%3.3d",$self->getTstNo()));
	}
}
sub setTstNam {
	my ($self,$szTstnam) = @_;
	
	$szTstnam = $self->_upcase_string($szTstnam);
	
	if ( defined($szTstnam) &&
	     ( length($szTstnam) > 0 ) )
	{
		$self->{'_tstnam'} = $szTstnam;
	}
	elsif ( defined($self->getTstNo()) )
	{
		$self->{'_tstnam'} = sprintf("%3.3d",$self->getTstNo());
	}	
	else
	{
		$self->{'_tstnam'} = "";
	}
}
sub setTitle {
	my ($self,$szTitle) = @_;
	
	$szTitle = $self->_upcase_string($szTitle);
	
	if ( defined($szTitle) &&
	     ( length($szTitle) > 0 ) )
	{
		$self->{'_title'} = $szTitle;
	}
	else
	{	
		$self->{'_title'} = "";
	}
}
sub setTstPrf {
	my ($self,$szTstprf) = @_;
	
	$szTstprf = $self->_upcase_string($szTstprf);
	
	if ( defined($szTstprf) &&
	     ( length($szTstprf) > 0 ) )
	{
		$self->{'_tstprf'} = $szTstprf;
	}
	else
	{	
		$self->{'_tstprf'} = "OTHER";
	}
}
sub setTstRef {
	my ($self,$szTstref) = @_;
	
	$szTstref = $self->_upcase_string($szTstref);
	
	if ( defined($szTstref) &&
	     ( length($szTstref) > 0 ) )
	{
		$self->{'_tstref'} = $szTstref;
	}
	else
	{	
		$self->{'_tstref'} = "";
	}
}
sub setTstCfn {
	my ($self,$szTstcfn) = @_;
	
	$szTstcfn = $self->_upcase_string($szTstcfn);
	
	if ( defined($szTstcfn) &&
	     ( length($szTstcfn) > 0 ) )
	{
		$self->{'_tstcfn'} = $szTstcfn;
	}
	else
	{	
		$self->{'_tstcfn'} = "OTH";
	}
}
sub setImpAng {
	my ($self,$nImpang) = @_;
	
	if ( defined($nImpang) )
	{
		$self->{'_impang'} = $nImpang;
	}
	else
	{	
		$self->{'_impang'} = 0;
	}
}
sub setClsSpd {
	my ($self,$fClsspd) = @_;
	
	if ( defined($fClsspd) )
	{
		$self->{'_clsspd'} = $fClsspd;
	}
	else
	{	
		$self->{'_clsspd'} = 0.0;
	}
}
sub getTstSrc {
	my $self = shift;
	
	return($self->{'_tstsrc'});
}
sub getTstNo {
	my $self = shift;
	
	return($self->{'_tstno'});
}
sub getTstNam {
	my $self = shift;
	
	return($self->{'_tstnam'});
}
sub getTitle {
	my $self = shift;
	
	return($self->{'_title'});
}
sub getTstPrf {
	my $self = shift;
	
	return($self->{'_tstprf'});
}
sub getTstRef {
	my $self = shift;
	
	return($self->{'_tstref'});
}
sub getTstCfn {
	my $self = shift;
	
	return($self->{'_tstcfn'});
}
sub getImpAng {
	my $self = shift;
	
	return($self->{'_impang'});
}
sub getClsSpd {
	my $self = shift;
	
	return($self->{'_clsspd'});
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
