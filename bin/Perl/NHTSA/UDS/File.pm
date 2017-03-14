package NHTSA::UDS::File;

use Config;
require NHTSA::UDS::SpecBase;

@ISA = ("NHTSA::UDS::SpecBase");

use strict;

########################################################################
#      CLASS: NHTSA::UDS::File.pm
#    PURPOSE: To encapsulate the data for the File information
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
	my $szFilename = shift;

	$self->setSysByteOrder();
	$self->setSysFPFormat();

	$self->setFilename($szFilename);
	$self->setFileVer("UDS-1992");
	$self->setNChan(1);
	$self->setFileForm("X-Y");
	$self->setNumForm("");
	$self->setDimSys("MET");
}

########################################################################
#
#	Accessors
#
########################################################################

sub setSysByteOrder {
	my $self = shift;

	my(@aTest,$nTest);

	$aTest[0] = 54;
	$aTest[1] = 53;
	$aTest[2] = 53;
	$aTest[3] = 51;

	$nTest = unpack("s1",pack("c4",@aTest));

	if ( $nTest == 13877 )
	{
		$self->{'_sys_byte_order'} = "MSB2LSB";
	}
	elsif ( $nTest == 13622 )
	{
		$self->{'_sys_byte_order'} = "LSB2MSB";
	}
	else
	{
		$self->{'_sys_byte_order'} = "UNKNOWN";
	}
}

sub setSysFPFormat {
	my $self = shift;
	my $szOS = "";
	
	( $szOS = $Config{'osname'}) =~ tr/A-Z/a-z/;

	if ( $szOS =~ /(dec_osf|mswin32|win32|linux)/ )
	{
		$self->{'_sys_fp_format'} = "IEEE";
	}
	elsif ( $szOS =~ /(irix|aix)/ )
	{
		$self->{'_sys_fp_format'} = "SUN";
	}
	elsif ( $szOS =~ /vms/ )
	{
		$self->{'_sys_fp_format'} = "VAX";
	}
	else
	{
		$self->{'_sys_fp_format'} = "UNKNOWN";
	}	
}
sub setFilename {
	my ($self,$szFilename) = @_;
	
	if ( defined($szFilename) &&
	     ( length($szFilename) > 0 ) )
	{
		$self->{'_filename'} = $szFilename;
	}
	else
	{
		$self->{'_filename'} = "";
	}
}
sub setFileVer {
	my ($self,$szFilever) = @_;
	
	$szFilever = $self->_upcase_string($szFilever);
	
	if ( defined($szFilever) &&
	     ( $szFilever =~ /UDS-1992/ ) )
	{
		$self->{'_filever'} = $szFilever;
	}
	else
	{
		$self->{'_filever'} = "UNKNOWN";
	}
}
sub setNChan {
	my ($self,$nNChan) = @_;
	
	if ( defined($nNChan) &&
	     ( $nNChan >= 1 ) )
	{
		$self->{'_nchan'} = $nNChan;
	}
	else
	{
		$self->{'_nchan'} = 1;
	}
}
sub setFileForm {
	my ($self,$szFileform) = @_;
	
	$szFileform = $self->_upcase_string($szFileform);
	
	if ( defined($szFileform) &&
	     ( length($szFileform) > 0 ) &&
	     ( $szFileform =~ /(Y|X-Y)/ ) )
	{
		$self->{'_fileform'} = $szFileform;
	}
	else
	{
		$self->{'_fileform'} = "";
	}
}
sub setNumForm {
	my ($self,$szNumform) = @_;
	
	$szNumform = $self->_upcase_string($szNumform);
	
	if ( defined($szNumform) &&
	     ( length($szNumform) > 0 ) &&
	     ( $szNumform =~ /(VAX|IEEE|SUN)/ ) )
	{
		$self->{'_numform'} = $szNumform;
	}
	else
	{
		$self->{'_numform'} = $self->getSysFPFormat();
	}
}
sub setDimSys {
	my ($self,$szDimsys) = @_;

	$szDimsys = $self->_upcase_string($szDimsys);
	
	if ( defined($szDimsys) &&
	     ( length($szDimsys) > 0 ) &&
	     ( $szDimsys =~ /(MET|SI|ENG)/ ) )
	{
		$self->{'_dimsys'} = $szDimsys;
	}
	else
	{
		$self->{'_dimsys'} = "";
	}
}
sub getSysByteOrder {
	my $self = shift;
	
	return($self->{'_sys_byte_order'});
}

sub getSysFPFormat {
	my $self = shift;
	
	return($self->{'_sys_fp_format'});
}
sub getFilename {
	my ($self) = shift;
	
	return($self->{'_filename'});
}
sub getFileVer {
	my ($self) = shift;
	
	return($self->{'_filever'});
}
sub getNChan {
	my ($self) = shift;
	
	return($self->{'_nchan'});
}
sub getFileForm {
	my ($self) = shift;
	
	return($self->{'_fileform'});
}
sub getNumForm {
	my ($self) = shift;
	
	return($self->{'_numform'});
}
sub getDimSys {
	my ($self) = shift;
	
	return($self->{'_dimsys'});
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
