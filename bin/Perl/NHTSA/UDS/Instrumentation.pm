package NHTSA::UDS::Instrumentation;

require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::Instrumentation.pm
#    PURPOSE: To encapsulate the data for the Instrumentation
#	      information associated with a UDS-1992 signal file
#     AUTHOR: Andrew Orndorff (andrew.orndorff@nhtsa.dot.gov)
#     SOURCE:
#    CREATED: 1999/03/24
#   MODIFIED: 2001/03/27  Stuart G. Mentzer (see # SGM comments)
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

	$self->setIChan(1);
	$self->setChanForm("X-Y");
	$self->setCurNo(0);
	$self->setCurNam(undef);
	$self->setSenAtt(undef);
	$self->setSenLoc(undef);
	$self->setSenNum(0);
	$self->setBandNo(0);
	$self->setGagNo(0);
	$self->setAxis(undef);
	$self->setYType(undef);
	$self->setYUnits(undef);
	$self->setXType("TIME");
	$self->setXUnits("SECONDS");
	$self->setStatus("COMPUTED");
	$self->setCurTyp("TIME SERIES");
	$self->setCurDsc("COMPUTED / SIMULATED DATA");
	$self->setNfp(0);
	$self->setNlp(0);
	$self->setDel(0.0);
	$self->setIniVel(0.0);
	$self->setPref(1650.0);
	$self->setFCut(0.0);
	$self->setFCor(0.0);
	$self->setFStp(0.0);
	$self->setSclFac(1.0);
	$self->setID1(0);
	$self->setID2(0);
	$self->setID3(0);
	$self->setID4(0);
	$self->setID5(0);
	$self->setRD1(0.0);
	$self->setRD2(0.0);
	$self->setRD3(0.0);
	$self->setRD4(0.0);
	$self->setRD5(0.0);
	$self->setCD1(undef);					
	$self->setCD2(undef);					
}

########################################################################
#
#	Accessors
#
########################################################################

sub setIChan {
	my ($self,$nIchan) = @_;

	if ( defined($nIchan) &&
	     ( $nIchan >= 1 ) )
	{
		$self->{'_ichan'} = $nIchan;
	}
	else
	{
		$self->{'_ichan'} = 1;
	}	
}
sub setChanForm {
	my ($self,$szChanform) = @_;
	
	$szChanform = $self->_upcase_string($szChanform);
	
	if ( $szChanform =~ /(Y|X-Y)/ )
	{
		$self->{'_chanform'} = $szChanform;
	}
	else
	{
		$self->{'_chanform'} = "UNKNOWN";
	}
}
sub setCurNo {
	my ($self,$nCurno) = @_;

	if ( defined($nCurno) &&
	     ( $nCurno >= 0 ) )
	{
		$self->{'_curno'} = $nCurno;
	}
	else
	{
		$self->{'_curno'} = 0;
	}
	
	if ( !defined($self->getCurNam()) )
	{
		$self->setCurNam(sprintf("%3.3d",$self->getCurNo()));
	}
}
sub setCurNam {
	my ($self,$szCurnam) = @_;

	$szCurnam = $self->_upcase_string($szCurnam);
		
	if ( defined($szCurnam) &&
	     ( length($szCurnam) > 0 ) )
	{
		$self->{'_curnam'} = $szCurnam;
	}
	elsif ( defined($self->getCurNo()) )
	{
		$self->{'_curnam'} = sprintf("%3.3d",$self->getCurNo());
	}	
	else
	{
		$self->{'_curnam'} = "";
	}
}
sub setSenAtt {
	my ($self,$szSenatt) = @_;
	
	$szSenatt = $self->_upcase_string($szSenatt);
	
	if ( defined($szSenatt) &&
	     ( length($szSenatt) > 0 ) )
	{
		$self->{'_senatt'} = $szSenatt;
	}
	else
	{
		$self->{'_senatt'} = "";
	}
}
sub setSenLoc {
	my ($self,$szSenloc) = @_;

	$szSenloc = $self->_upcase_string($szSenloc);
		
	if ( defined($szSenloc) &&
	     ( length($szSenloc) > 0 ) )
	{
		$self->{'_senloc'} = $szSenloc;
	}
	else
	{
		$self->{'_senloc'} = "";
	}
}
sub setSenNum {
	my ($self,$nSennum) = @_;
	
	if ( defined($nSennum) &&
	     ( $nSennum >= 0 ) )
	{
		$self->{'_sennum'} = $nSennum;
	}
	else
	{
		$self->{'_sennum'} = 0;
	}	
}
sub setBandNo {
	my ($self,$nBandno) = @_;
	
	if ( defined($nBandno) &&
	     ( $nBandno >= 0 ) )
	{
		$self->{'_bandno'} = $nBandno;
	}
	else
	{
		$self->{'_bandno'} = 0;
	}	
}
sub setGagNo {
	my ($self,$nGagno) = @_;
	
	if ( defined($nGagno) &&
	     ( $nGagno >= 0 ) )
	{
		$self->{'_gagno'} = $nGagno;
	}
	else
	{
		$self->{'_gagno'} = 0;
	}	
}
sub setAxis {
	my ($self,$szAxis) = @_;

	$szAxis = $self->_upcase_string($szAxis);
		
	if ( defined($szAxis) &&
	     ( length($szAxis) > 0 ) &&
	     ( $szAxis =~ /(XL|YL|ZL|XG|YG|ZG|X|Y|Z|OT|NA)/ ) )
	{
		$self->{'_axis'} = $szAxis;
	}
	else
	{
		$self->{'_axis'} = "";
	}
}
sub setYType {
	my ($self,$szYtype) = @_;

	$szYtype = $self->_upcase_string($szYtype);
		
	if ( defined($szYtype) &&
	     ( length($szYtype) > 0 ) )
	{
		$self->{'_ytype'} = $szYtype;
	}
	else
	{
		$self->{'_ytype'} = "";
	}
}
sub setYUnits {
	my ($self,$szYunits) = @_;

	$szYunits = $self->_upcase_string($szYunits);
		
	if ( defined($szYunits) &&
	     ( length($szYunits) > 0 ) )
	{
		$self->{'_yunits'} = $szYunits;
	}
	else
	{
		$self->{'_yunits'} = "";
	}
}
sub setXType {
	my ($self,$szXtype) = @_;

	$szXtype = $self->_upcase_string($szXtype);
		
	if ( defined($szXtype) &&
	     ( length($szXtype) > 0 ) )
	{
		$self->{'_xtype'} = $szXtype;
	}
	elsif ( ( $self->getDel() > 0.0 ) &&
	        ( $self->getXUnits() =~ /(HOURS|MINUTES|SECONDS)/ ) )
	{
		$self->{'_xtype'} = "TIME";
	}
	else
	{
		$self->{'_xtype'} = "UNKNOWN";
	}
}
sub setXUnits {
	my ($self,$szXunits) = @_;
	
	$szXunits = $self->_upcase_string($szXunits);	
	
	if ( defined($szXunits) &&
	     ( length($szXunits) > 0 ) )
	{
		$self->{'_xunits'} = $szXunits;
	}
	else
	{
		$self->{'_xunits'} = "";
	}
}
sub setStatus {
	my ($self,$szStatus) = @_;

	$szStatus = $self->_upcase_string($szStatus);
		
	if ( defined($szStatus) &&
	     ( length($szStatus) > 0 ) )
	{
		$self->{'_status'} = $szStatus;
	}
	else
	{
		$self->{'_status'} = "";
	}
}
sub setCurTyp {
	my ($self,$szCurtyp) = @_;

	$szCurtyp = $self->_upcase_string($szCurtyp);
		
	if ( defined($szCurtyp) &&
	     ( length($szCurtyp) > 0 ) )
	{
		$self->{'_curtyp'} = $szCurtyp;
	}
	elsif ( defined($self->getXType()) &&
		( $self->getXType() =~ /TIME/ ) )
	{
		$self->{'_curtyp'} = "TIME SERIES";
	}
	else
	{
		$self->{'_curtyp'} = "";
	}
}
sub setCurDsc {
	my ($self,$szCurdsc) = @_;

	$szCurdsc = $self->_upcase_string($szCurdsc);
		
	if ( defined($szCurdsc) &&
	     ( length($szCurdsc) > 0 ) )
	{
		$self->{'_curdsc'} = $szCurdsc;
	}
	else
	{
		$self->{'_curdsc'} = "";
	}	
}
sub setNfp {
	my ($self,$nNfp) = @_;
	
	$self->{'_nfp'} = $nNfp; # SGM
}
sub setNlp {
	my ($self,$nNlp) = @_;
	
	$self->{'_nlp'} = $nNlp; # SGM
}
sub setDel {
	my ($self,$fDel) = @_;
	
	if ( defined($fDel) )
	{
		$self->{'_del'} = sprintf("%12.5E",$fDel);
	}
	else
	{
		$self->{'_del'} = 0.0;
	}	
}
sub setIniVel {
	my ($self,$fInivel) = @_;
	
	if ( defined($fInivel) )
	{
		$self->{'_inivel'} = sprintf("%12.5E",$fInivel);
	}
	else
	{
		$self->{'_inivel'} = 0.0;
	}	
}
sub setPref {
	my ($self,$fPref) = @_;
	
	if ( defined($fPref) )
	{
		$self->{'_pref'} = sprintf("%12.5E",$fPref);
	}
	else
	{
		$self->{'_pref'} = 1650.0;
	}	
}
sub setFCut {
	my ($self,$fFcut) = @_;
	
	if ( defined($fFcut) )
	{
		$self->{'_fcut'} = sprintf("%12.5E",$fFcut);
	}
	else
	{
		$self->{'_fcut'} = 0.0;
	}	
}
sub setFCor {
	my ($self,$fFcor) = @_;
	
	if ( defined($fFcor) )
	{
		$self->{'_fcor'} = sprintf("%12.5E",$fFcor);
	}
	else
	{
		$self->{'_fcor'} = 0.0;
	}	
}
sub setFStp {
	my ($self,$fFstp) = @_;
	
	if ( defined($fFstp) )
	{
		$self->{'_fstp'} = sprintf("%12.5E",$fFstp);
	}
	else
	{
		$self->{'_fstp'} = 0.0;
	}	
}
sub setSclFac {
	my ($self,$fSclfac) = @_;
	
	if ( defined($fSclfac) )
	{
		$self->{'_sclfac'} = sprintf("%12.5E",$fSclfac);
	}
	else
	{
		$self->{'_sclfac'} = 1.0;
	}	
}
sub setID1 {
	my ($self,$nID1) = @_;
	
	if ( defined($nID1) )
	{
		$self->{'_id1'} = sprintf("%d",$nID1);
	}
	else
	{
		$self->{'_id1'} = 0;
	}	
}
sub setID2 {
	my ($self,$nID2) = @_;
	
	if ( defined($nID2) )
	{
		$self->{'_id2'} = sprintf("%d",$nID2);
	}
	else
	{
		$self->{'_id2'} = 0;
	}	
}
sub setID3 {
	my ($self,$nID3) = @_;
	
	if ( defined($nID3) )
	{
		$self->{'_id3'} = sprintf("%d",$nID3);
	}
	else
	{
		$self->{'_id3'} = 0;
	}	
}
sub setID4 {
	my ($self,$nID4) = @_;
	
	if ( defined($nID4) )
	{
		$self->{'_id4'} = sprintf("%d",$nID4);
	}
	else
	{
		$self->{'_id4'} = 0;
	}	
}
sub setID5 {
	my ($self,$nID5) = @_;
	
	if ( defined($nID5) )
	{
		$self->{'_id5'} = sprintf("%d",$nID5);
	}
	else
	{
		$self->{'_id5'} = 0;
	}	
}
sub setRD1 {
	my ($self,$fRD1) = @_;
	
	if ( defined($fRD1) )
	{
		$self->{'_rd1'} = sprintf("%12.5E",$fRD1);
	}
	else
	{
		$self->{'_rd1'} = 0.0;
	}	
}
sub setRD2 {
	my ($self,$fRD2) = @_;
	
	if ( defined($fRD2) )
	{
		$self->{'_rd2'} = sprintf("%12.5E",$fRD2);
	}
	else
	{
		$self->{'_rd2'} = 0.0;
	}	
}
sub setRD3 {
	my ($self,$fRD3) = @_;
	
	if ( defined($fRD3) )
	{
		$self->{'_rd3'} = sprintf("%12.5E",$fRD3);
	}
	else
	{
		$self->{'_rd3'} = 0.0;
	}	
}
sub setRD4 {
	my ($self,$fRD4) = @_;
	
	if ( defined($fRD4) )
	{
		$self->{'_rd4'} = sprintf("%12.5E",$fRD4);
	}
	else
	{
		$self->{'_rd4'} = 0.0;
	}	
}
sub setRD5 {
	my ($self,$fRD5) = @_;
	
	if ( defined($fRD5) )
	{
		$self->{'_rd5'} = sprintf("%12.5E",$fRD5);
	}
	else
	{
		$self->{'_rd5'} = 0.0;
	}	
}
sub setCD1 {
	my ($self,$szCD1) = @_;

	$szCD1 = $self->_upcase_string($szCD1);
		
	if ( defined($szCD1) &&
	     ( length($szCD1) > 0 ) )
	{
		$self->{'_cd1'} = $szCD1;
	}
	else
	{
		$self->{'_cd1'} = "";
	}
}
sub setCD2 {
	my ($self,$szCD2) = @_;
	
	$szCD2 = $self->_upcase_string($szCD2);
		
	if ( defined($szCD2) &&
	     ( length($szCD2) > 0 ) )
	{
		$self->{'_cd2'} = $szCD2;
	}
	else
	{
		$self->{'_cd2'} = "";
	}
}
sub getIChan {
	my $self = shift;
	
	return($self->{'_ichan'});
}
sub getChanForm {
	my $self = shift;
	
	return($self->{'_chanform'});
}
sub getCurNo {
	my $self = shift;
	
	return($self->{'_curno'});
}
sub getCurNam {
	my $self = shift;
	
	return($self->{'_curnam'});
}
sub getSenAtt {
	my $self = shift;
	
	return($self->{'_senatt'});
}
sub getSenLoc {
	my $self = shift;
	
	return($self->{'_senloc'});
}
sub getSenNum {
	my $self = shift;
	
	return($self->{'_sennum'});
}
sub getBandNo {
	my $self = shift;
	
	return($self->{'_bandno'});
}
sub getGagNo {
	my $self = shift;
	
	return($self->{'_gagno'});
}
sub getAxis {
	my $self = shift;
	
	return($self->{'_axis'});
}
sub getYType {
	my $self = shift;
	
	return($self->{'_ytype'});
}
sub getYUnits {
	my $self = shift;
	
	return($self->{'_yunits'});
}
sub getXType {
	my $self = shift;
	
	return($self->{'_xtype'});
}
sub getXUnits {
	my $self = shift;
	
	return($self->{'_xunits'});
}
sub getStatus {
	my $self = shift;
	
	return($self->{'_status'});
}
sub getCurTyp {
	my $self = shift;
	
	return($self->{'_curtyp'});
}
sub getCurDsc {
	my $self = shift;
	
	return($self->{'_curdsc'});
}
sub getNfp {
	my $self = shift;
	
	return($self->{'_nfp'});
}
sub getNlp {
	my $self = shift;
	
	return($self->{'_nlp'});
}
sub getDel {
	my $self = shift;
	
	return($self->{'_del'});
}
sub getIniVel {
	my $self = shift;
	
	return($self->{'_inivel'});
}
sub getPref {
	my $self = shift;
	
	return($self->{'_pref'});
}
sub getFCut {
	my $self = shift;
	
	return($self->{'_fcut'});
}
sub getFCor {
	my $self = shift;
	
	return($self->{'_fcor'});
}
sub getFStp {
	my $self = shift;
	
	return($self->{'_fstp'});
}
sub getSclFac {
	my $self = shift;
	
	return($self->{'_sclfac'});
}
sub getID1 {
	my $self = shift;
	
	return($self->{'_id1'});
}
sub getID2 {
	my $self = shift;
	
	return($self->{'_id2'});
}
sub getID3 {
	my $self = shift;
	
	return($self->{'_id3'});
}
sub getID4 {
	my $self = shift;
	
	return($self->{'_id4'});
}
sub getID5 {
	my $self = shift;
	
	return($self->{'_id5'});
}
sub getRD1 {
	my $self = shift;
	
	return($self->{'_rd1'});
}
sub getRD2 {
	my $self = shift;
	
	return($self->{'_rd2'});
}
sub getRD3 {
	my $self = shift;
	
	return($self->{'_rd3'});
}
sub getRD4 {
	my $self = shift;
	
	return($self->{'_rd4'});
}
sub getRD5 {
	my $self = shift;
	
	return($self->{'_rd5'});
}
sub getCD1 {
	my $self = shift;
	
	return($self->{'_cd1'});
}
sub getCD2 {
	my $self = shift;
	
	return($self->{'_cd2'});
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
