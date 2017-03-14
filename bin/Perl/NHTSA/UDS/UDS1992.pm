package NHTSA::UDS::UDS1992;

use NHTSA::UDS::File;
use NHTSA::UDS::Test;
use NHTSA::UDS::Vehicle;
use NHTSA::UDS::Occupant;
use NHTSA::UDS::Injury;
use NHTSA::UDS::Instrumentation;

use strict;

########################################################################
#      CLASS: UDS1992.pm
#    PURPOSE: To encapsulate the data and methods for manipulating NHTSA
#	      UDS1992 crash test signal files
#     AUTHOR: Andrew Orndorff (andrew.orndorff@nhtsa.dot.gov)
#     SOURCE:
#    CREATED: 1999/03/23
#   MODIFIED: 2003/01/13  Stuart G. Mentzer (see # SGM comments)
########################################################################


########################################################################
#
#	Package Constants
#
########################################################################

my $VERSION = "1.27";

my $RECORDLENGTH = 512;
my $HEADERLENGTH = 12;
my $DATARECORDLENGTH = $RECORDLENGTH - $HEADERLENGTH;

########################################################################
#
#	Constructor
#
########################################################################

sub new {
	my($class,@param) = @_;

	my $self = {};

	bless $self,ref($class) || $class;

	$self->{FILE} = NHTSA::UDS::File->new();
	$self->{TEST} = NHTSA::UDS::Test->new();
	$self->{VEHICLE} = NHTSA::UDS::Vehicle->new();
	$self->{OCCUPANT} = NHTSA::UDS::Occupant->new();
	$self->{INJURY} = NHTSA::UDS::Injury->new();
	$self->{COMPONENT2} = NHTSA::UDS::Vehicle->new();
	$self->{INSTRUMENTATION} = NHTSA::UDS::Instrumentation->new();

	$self->_initialize(@param);

	$self;
}

sub _initialize {
	my ($self,$szFilename) = @_;

	$self->file->_initialize($szFilename);
	$self->test->_initialize();
	$self->vehicle->_initialize();
	$self->occupant->_initialize();
	$self->injury->_initialize();
	$self->component2->_initialize();
	$self->instrumentation->_initialize();

	$self->setXData(undef);
	$self->setYData(undef);

	$self->_setArraySize(0);
	$self->_setLastIndex(0);
}

sub file {
	my $self = shift;

	return($self->{FILE});
}

sub test {
	my $self = shift;

	return($self->{TEST});
}

sub vehicle {
	my $self = shift;

	return($self->{VEHICLE});
}

sub occupant {
	my $self = shift;

	return($self->{OCCUPANT});
}

sub injury {
	my $self = shift;

	return($self->{INJURY});
}

sub component2 {
	my $self = shift;

	return($self->{COMPONENT2});
}

sub occupant2 {
	my $self = shift;

	return($self->{OCCUPANT2});
}

sub instrumentation {
	my $self = shift;

	return($self->{INSTRUMENTATION});
}

########################################################################
#
#	Accessors
#
########################################################################

sub setFullRead {
	my ($self,$bFullread) = @_;

	if ( defined($bFullread) )
	{
		if ( $bFullread == 1 )
		{
			$self->{'_fullread'} = 1;
		}
		elsif ( $bFullread == 0 )
		{
			$self->{'_fullread'} = 0;
		}
		else
		{
			$self->{'_fullread'} = 1;
		}
	}
	else
	{
		$self->{'_fullread'} = 1;
	}
}

sub setXData {
	my $self = shift;
	my @afXData = @_;

	if ( @afXData )
	{
		@{ $self->{'_XData'} } = @afXData;
	}
	else
	{
		@{ $self->{'_XData'} } = [];
	}
}

sub setYData {
	my $self = shift;
	my @afYData = @_;

	if ( @afYData )
	{
		@{ $self->{'_YData'} } = @afYData;
	}
	else
	{
		@{ $self->{'_YData'} } = [];
	}
}

sub _setArraySize {
	my ($self,$nSize) = @_;

	if ( defined($nSize) &&
	     ( $nSize >= 0 ) )
	{
		$self->{'_array_size'} = $nSize;
	}
	else
	{
		$self->{'_array_size'} = 0;
	}
}

sub _setLastIndex {
	my ($self,$nLastIndex) = @_;

	if ( defined($nLastIndex) )
	{
		$self->{'_last_index'} = $nLastIndex;
	}
	else
	{
		$self->{'_last_index'} = 0;
	}
}

sub _setByteOrderHash {
	my ( $self,$nHash ) = @_;

	if ( defined($nHash) )
	{
		$self->{'_bo_hash'} = $nHash;
	}
	else
	{
		$self->{'_bo_hash'} = 0;
	}
}

sub _setFPFormatHash {
	my ( $self,$nHash ) = @_;

	if ( defined($nHash) )
	{
		$self->{'_fp_hash'} = $nHash;
	}
	else
	{
		$self->{'_fp_hash'} = 0;
	}
}

sub getFullRead {
	my $self = shift;

	return($self->{'_fullread'});
}

sub getXData {
	my $self = shift;

	return( @{ $self->{'_XData'} } );
}

sub getYData {
	my $self = shift;

	return( @{ $self->{'_YData'} } );
}

sub getArraySize {
	my $self = shift;

	return($self->{'_array_size'});
}

sub getLastIndex {
	my $self = shift;

	return($self->{'_last_index'});
}

sub _getByteOrderHash {
	my $self = shift;

	return($self->{'_bo_hash'});
}

sub _getFPFormatHash {
	my $self = shift;

	return($self->{'_fp_hash'});
}

########################################################################
#
#	Public Instance Methods
#
########################################################################

sub is_UDS { # SGM
	my $self = shift;
	my $szFile = shift;

	my $nMMOffset = 0;
	my $szMMAP = "";
	my $nFileSize = 0;
	my $nRecordCount = 0;
	my $szBuffer;
	my @aBuffer = [];
	my @aStatBuf;
	my $szInFile;
	my $bFullRead;

#
# Determine if to read all data
#
	$bFullRead = $self->getFullRead();
#
# Get input source
#
	if ( !defined($szFile) )
	{
		$szInFile = $self->file->getFilename();
	}
	else
	{
		$szInFile = $szFile;
	}
#
# If we have been passed a handle as an argument, then we want to read
# from that handle (Eg. socket, etc)
#
	if ( ref($szInFile) )
	{
		while ( read(*$szInFile,$szBuffer,$RECORDLENGTH) )
		{
			$szMMAP = "$szMMAP"."$szBuffer";
			$nRecordCount++;
		}
		$nFileSize = $nRecordCount * $RECORDLENGTH;
		$nRecordCount = 0;
		if ( $nFileSize == 0 )
		{
			return(0);
		}
	}
#
# Read the input file into a memory buffer
#
	elsif ( -r $szInFile )
	{
		@aStatBuf = stat($szInFile);
		$nFileSize = $aStatBuf[7];
		open(HINPUT,"<$szInFile") || return(0);
		binmode HINPUT;
		read(HINPUT,$szMMAP,$nFileSize);
		close(HINPUT);
	}
	else
	{
		$self->_do_warning("Unable to open input $szInFile");
		return(0);
	}
#
# Read the first record
#
	$szBuffer = "";

	$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);

	@aBuffer = unpack("a$HEADERLENGTH"."a4a10a5a3a15a4a12a70a25a10a3a4a4a4a2a30a15a25a4a25a4a4a4a6a4a1a4a2a20a20a4a4a4a4a4a4a1",$szBuffer);
#
# Check the file version
#
	if ( $aBuffer[0] !~ /^UDS-1992/ )
	{
		return(0);
	}
	return(1);
}

sub read {
	my $self = shift;
	my $szFile = shift;

	my @aYData = [];
	my @aXData = [];
	my $nXIndex = 0;
	my $nYIndex = 0;
	my $nMMOffset = 0;
	my $szMMAP = "";
	my $nFileSize = 0;
	my $nRecordCount = 0;
	my $nNfp = 0;
	my $nNlp = 0;
	my $fDel = 0.0;
	my $nLastIndex = 0;
	my $szBuffer;
	my @aBuffer = [];
	my $szRecHdr;
	my @aStatBuf;
	my $szInFile;
	my $bFullRead;
	my $npoint = 0;
	my $i = 0;

#
# Determine if to read all data
#
	$bFullRead = $self->getFullRead();
#
# Get input source
#
	if ( !defined($szFile) )
	{
		$szInFile = $self->file->getFilename();
	}
	else
	{
		$szInFile = $szFile;
	}
#
# If we have been passed a handle as an argument, then we want to read
# from that handle (Eg. socket, etc)
#
	if ( ref($szInFile) )
	{
		while ( read(*$szInFile,$szBuffer,$RECORDLENGTH) )
		{
			$szMMAP = "$szMMAP"."$szBuffer";
			$nRecordCount++;
		}
		$nFileSize = $nRecordCount * $RECORDLENGTH;
		$nRecordCount = 0;
		if ( $nFileSize == 0 )
		{
			return(0);
		}
	}
#
# Read the input file into a memory buffer
#
	elsif ( -r $szInFile )
	{
		@aStatBuf = stat($szInFile);
		$nFileSize = $aStatBuf[7];
		open(HINPUT,"<$szInFile") || return(0);
		binmode HINPUT;
		read(HINPUT,$szMMAP,$nFileSize);
		close(HINPUT);
	}
	else
	{
		$self->_do_warning("Unable to open input $szInFile");
		return(0);
	}
#
# Read the first record
#
	$szBuffer = "";

	$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);

	@aBuffer = unpack("a$HEADERLENGTH"."a4a10a5a3a15a4a12a70a25a10a3a4a4a4a2a30a15a25a4a25a4a4a4a6a4a1a4a2a20a20a4a4a4a4a4a4a1",$szBuffer);
#
# Check the file version
#
	if ( $aBuffer[0] !~ /^UDS-1992/ )
	{
		$self->_do_warning("Not a UDS-1992 file - version $aBuffer[0]");
		return(0);
	}
#
# Assign data
#
	$self->file->setFileVer($aBuffer[0]);
#
# Must assign NumForm before any integer or floating point conversions
# in order to make sure that they go properly
#
	$self->file->setNumForm($aBuffer[3]);
#
# Compute byte order and floating point format hashes
#
	$self->_setByteOrderHash($self->file->getNumForm().$self->file->getSysByteOrder());
	$self->_setFPFormatHash($self->file->getNumForm().$self->file->getSysFPFormat());

#
# Convert the remainder of this section of data
#
	$self->file->setNChan($self->_integer_convert($aBuffer[1]));
	$self->file->setFileForm($aBuffer[2]);
	$self->file->setDimSys($aBuffer[4]);

	if ( $bFullRead == 1 )
	{
		$self->test->setTstSrc($aBuffer[5]);
		$self->test->setTstNo($self->_integer_convert($aBuffer[6]));
		$self->test->setTstNam($aBuffer[7]);
		$self->test->setTitle($aBuffer[8]);
		$self->test->setTstPrf($aBuffer[9]);
		$self->test->setTstRef($aBuffer[10]);
		$self->test->setTstCfn($aBuffer[11]);
		$self->test->setImpAng($self->_integer_convert($aBuffer[12]));
		$self->test->setClsSpd($self->_float_convert($aBuffer[13]));

		$self->vehicle->setCmpNo($self->_integer_convert($aBuffer[14]));
		$self->vehicle->setCmpTyp($aBuffer[15]);
		$self->vehicle->setCmpDsc($aBuffer[16]);
		$self->vehicle->setMake($aBuffer[17]);
		$self->vehicle->setModel($aBuffer[18]);
		$self->vehicle->setYear($self->_integer_convert($aBuffer[19]));
		$self->vehicle->setBody($aBuffer[20]);
		$self->vehicle->setEngine($aBuffer[21]);
		$self->vehicle->setCmpWt($self->_float_convert($aBuffer[22]));
		$self->vehicle->setCmpSpd($self->_float_convert($aBuffer[23]));

		$self->occupant->setOccTyp($aBuffer[24]);
		$self->occupant->setOccAge($self->_float_convert($aBuffer[25]));
		$self->occupant->setOccSex($aBuffer[26]);
		$self->occupant->setOccWt($self->_float_convert($aBuffer[27]));
		$self->occupant->setDumSiz($aBuffer[28]);
		$self->occupant->setRestr1($aBuffer[29]);
		$self->occupant->setRestr2($aBuffer[30]);

		$self->injury->setHIC($self->_float_convert($aBuffer[31]));
		$self->injury->setT1($self->_float_convert($aBuffer[32]));
		$self->injury->setT2($self->_float_convert($aBuffer[33]));
		$self->injury->setHICDTUP($self->_float_convert($aBuffer[34]));
		$self->injury->setClip3M($self->_float_convert($aBuffer[35]));
		$self->injury->setCSI($self->_float_convert($aBuffer[36]));
		$self->injury->setAIS($aBuffer[37]);
	}
#
# Increment record count and adjust offset into memory buffer
#
	$nRecordCount++;
	$nMMOffset = $nRecordCount * $RECORDLENGTH;
#
# Read the next record - either a CURVE HDR or an optional CMP-2 HDR
#
	$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);

	$szRecHdr = unpack("a$HEADERLENGTH",$szBuffer);
	if ( $szRecHdr =~ /CMP-2 HDR/ )
	{
		if ( $bFullRead == 1 )
		{
			@aBuffer = unpack("a$HEADERLENGTH"."a4a2a30a15a25a4a25a4a4a4",$szBuffer);
#
# Assign data
#
			$self->component2->setCmpNo($self->_integer_convert($aBuffer[1]));
			$self->component2->setCmpTyp($aBuffer[2]);
			$self->component2->setCmpDsc($aBuffer[3]);
			$self->component2->setMake($aBuffer[4]);
			$self->component2->setModel($aBuffer[5]);
			$self->component2->setYear($self->_integer_convert($aBuffer[6]));
			$self->component2->setBody($aBuffer[7]);
			$self->component2->setEngine($aBuffer[8]);
			$self->component2->setCmpWt($self->_float_convert($aBuffer[9]));
			$self->component2->setCmpSpd($self->_float_convert($aBuffer[10]));
		}
#
# Increment record count and adjust offset into memory buffer
#

		$nRecordCount++;
		$nMMOffset = $nRecordCount * $RECORDLENGTH;

		$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);
	}
	elsif ( $szRecHdr =~ /CURVE HDR/)
	{
	}
	else
	{
		$self->_do_warning("Unrecognized record header $szRecHdr");
		return(0);
	}
#
# Parse curve header
#
	@aBuffer = unpack("a$HEADERLENGTH"."a4a10a4a12a30a2a4a4a4a2a20a20a20a20a20a20a70a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a20a70",$szBuffer);
#
# Assign data
#
#
# Mandatory processing
#
	$self->instrumentation->setChanForm($aBuffer[2]);
	$self->instrumentation->setNfp($self->_integer_convert($aBuffer[18]));
	$self->instrumentation->setNlp($self->_integer_convert($aBuffer[19]));
	$self->instrumentation->setDel($self->_float_convert($aBuffer[20]));

	if ( $bFullRead == 1 )
	{
		$self->instrumentation->setIChan($self->_integer_convert($aBuffer[1]));
		$self->instrumentation->setCurNo($self->_integer_convert($aBuffer[3]));
		$self->instrumentation->setCurNam($aBuffer[4]);
		$self->instrumentation->setSenAtt($aBuffer[5]);
		$self->instrumentation->setSenLoc($aBuffer[6]);
		$self->instrumentation->setSenNum($self->_integer_convert($aBuffer[7]));
		$self->instrumentation->setBandNo($self->_integer_convert($aBuffer[8]));
		$self->instrumentation->setGagNo($self->_integer_convert($aBuffer[9]));
		$self->instrumentation->setAxis($aBuffer[10]);
		$self->instrumentation->setYType($aBuffer[11]);
		$self->instrumentation->setYUnits($aBuffer[12]);
		$self->instrumentation->setXType($aBuffer[13]);
		$self->instrumentation->setXUnits($aBuffer[14]);
		$self->instrumentation->setStatus($aBuffer[15]);
		$self->instrumentation->setCurTyp($aBuffer[16]);
		$self->instrumentation->setCurDsc($aBuffer[17]);
		$self->instrumentation->setIniVel($self->_float_convert($aBuffer[21]));
		$self->instrumentation->setPref($self->_float_convert($aBuffer[22]));
		$self->instrumentation->setFCut($self->_float_convert($aBuffer[23]));
		$self->instrumentation->setFCor($self->_float_convert($aBuffer[24]));
		$self->instrumentation->setFStp($self->_float_convert($aBuffer[25]));
		$self->instrumentation->setSclFac($self->_float_convert($aBuffer[26]));
		$self->instrumentation->setID1($self->_integer_convert($aBuffer[28]));
		$self->instrumentation->setID2($self->_integer_convert($aBuffer[29]));
		$self->instrumentation->setID3($self->_integer_convert($aBuffer[30]));
		$self->instrumentation->setID4($self->_integer_convert($aBuffer[31]));
		$self->instrumentation->setID5($self->_integer_convert($aBuffer[32]));
		$self->instrumentation->setRD1($self->_float_convert($aBuffer[33]));
		$self->instrumentation->setRD2($self->_float_convert($aBuffer[34]));
		$self->instrumentation->setRD3($self->_float_convert($aBuffer[35]));
		$self->instrumentation->setRD4($self->_float_convert($aBuffer[36]));
		$self->instrumentation->setRD5($self->_float_convert($aBuffer[37]));
		$self->instrumentation->setCD1($aBuffer[38]);
		$self->instrumentation->setCD2($aBuffer[39]);
	}


	$nNfp = $self->instrumentation->getNfp();
	$nNlp = $self->instrumentation->getNlp();
	$fDel = $self->instrumentation->getDel();

	$self->_setArraySize($nNlp - $nNfp + 1);

	$self->_setLastIndex($self->getArraySize() - 1);

	$nLastIndex = $self->getLastIndex();
#
# Increment record count and adjust offset into memory buffer
#
	$nRecordCount++;
	$nMMOffset = $nRecordCount * $RECORDLENGTH;
#
# Validity check - FileForm and ChanForm must be the same
#
	if ( $self->file->getFileForm() ne $self->instrumentation->getChanForm() )
	{
		$self->_do_warning("Invalid UDS-1992 file : FileForm != ChanForm \"".$self->file->getFileForm()."\" != \"".$self->instrumentation->getChanForm()."\"");
		return(0);
	}

#
# Branch based on szChanForm
#
	if ( $self->file->getFileForm() =~ /X-Y/ )
	{
		while ( $nMMOffset < $nFileSize )
		{
			$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);

			($szRecHdr,@aBuffer) = unpack("a$HEADERLENGTH"."c$DATARECORDLENGTH",$szBuffer);

			$i = 0;
			$npoint = 0;
			if ( $szRecHdr =~ /Y DATA/ )
			{
				while ( ( $npoint < 125 ) &&
					( $nYIndex <= $nLastIndex ) )
				{
					$aYData[$nYIndex] = $self->_float_convert($aBuffer[$i],$aBuffer[$i+1],$aBuffer[$i+2],$aBuffer[$i+3]);
					$nYIndex++;
					$npoint++;
					$i=$i+4;
				}
			}
			elsif ( $szRecHdr =~ /X DATA/ )
			{
				while ( ( $npoint < 125 ) &&
					( $nXIndex <= $nLastIndex ) )
				{
					$aXData[$nXIndex] = $self->_float_convert($aBuffer[$i],$aBuffer[$i+1],$aBuffer[$i+2],$aBuffer[$i+3]);
					$nXIndex++;
					$npoint++;
					$i=$i+4;
				}
			}
			else
			{
				$self->_do_warning("Unrecognized data header $szRecHdr");
				return(0);
			}
			$nRecordCount++;
			$nMMOffset = $nRecordCount * $RECORDLENGTH;
		}
	}
	elsif ( $self->file->getFileForm() =~ /Y/ )
	{
		while ( $nMMOffset < $nFileSize )
		{
			$szBuffer = substr($szMMAP,$nMMOffset,$RECORDLENGTH);

			($szRecHdr,@aBuffer) = unpack("a$HEADERLENGTH"."c$DATARECORDLENGTH",$szBuffer);

			$i = 0;
			$npoint = 0;
			if ( $szRecHdr =~ /Y DATA/ )
			{
				while ( ( $npoint < 125 ) &&
					( $nYIndex <= $nLastIndex ) )
				{
					$aYData[$nYIndex] = $self->_float_convert($aBuffer[$i],$aBuffer[$i+1],$aBuffer[$i+2],$aBuffer[$i+3]);
					$aXData[$nXIndex] = $fDel * ( $nXIndex + $nNfp );
					$nYIndex++;
					$nXIndex++;
					$npoint++;
					$i=$i+4;
				}
			}
			else
			{
				$self->_do_warning("Unrecognized data header $szRecHdr");
				return(0);
			}
			$nRecordCount++;
			$nMMOffset = $nRecordCount * $RECORDLENGTH;
		}
	}
	else
	{
		$self->_do_warning("Unrecognized channel format $self->instrumentation->getChanForm()");
		return(0);
	}
#
# Set object data arrays
#
	$self->setXData(@aXData);
	$self->setYData(@aYData);
#
# Return
#
	return(1);
}

########################################################################
#
#	Private Instance Methods
#
########################################################################

# Report a warning message to STDERR
sub _do_warning {
	my ($self,$szMessage) = @_;

	print STDERR "*** WARNING ***: $szMessage\n";
}

# Convert a UDS-1992 integer in the NUMFORM byte order to the local byte order
sub _integer_convert {
	my ($self,$szPoint) = @_;
	my $nConverted = 0;
	my @aBytes = [];
	my $szByte;
	my $nHash = 0;

	if ( !defined($szPoint) )
	{
		return($nConverted);
	}

	$nHash = $self->{'_bo_hash'};

	@aBytes = unpack("c4",$szPoint);

#
# IEEE -> MSB2LSB
# VAX -> MSB2LSB
# SUN -> LSB2MSB
#
	if ( ( $nHash =~ /^IEEEMSB2LSB$/ ) ||
	     ( $nHash =~ /^VAXMSB2LSB$/ ) ||
	     ( $nHash =~ /^SUNLSB2MSB$/ ) )
	{
		$szByte = $aBytes[0];
		$aBytes[0] = $aBytes[3];
		$aBytes[3] = $szByte;
		$szByte = $aBytes[1];
		$aBytes[1] = $aBytes[2];
		$aBytes[2] = $szByte;

		$szPoint = pack("c4",@aBytes);
		$nConverted = unpack("l1",$szPoint);
	}
#
# IEEE -> LSB2MSB
# VAX -> LSB2MSB
# SUN -> MSB2LSB
#
	elsif ( ( $nHash =~ /^IEEELSB2MSB$/ ) ||
		( $nHash =~ /^VAXLSB2MSB$/ ) ||
		( $nHash =~ /^SUNMSB2LSB$/ ) )
	{
		$nConverted = unpack("i1",$szPoint); # SGM: changed "s1" to "i1"
	}
	else
	{
		$nConverted = 0;
	}

	if ( defined($nConverted) &&
             ( $nConverted == -4294962697 ) )
	{
		$nConverted = unpack("i1",$szPoint); # SGM: changed "s1" to "i1"
	}

	return($nConverted);
}

# Convert a UDS-1992 floating point number in NUMFORM format to the local floating point format
sub _float_convert {
	my($self,@aBytes) = @_;
	my $rConverted = 0.0;
	my $szByte;
	my $nHash = 0;

	if ( ! @aBytes )
	{
		return($rConverted);
	}

	if ( $#aBytes == 0 )
	{
		@aBytes = unpack("c4",$aBytes[0]);
	}
	elsif ( $#aBytes != 3 )
	{
		return($rConverted);
	}

	$nHash = $self->{'_fp_hash'};

#
# IEEE -> IEEE
# VAX -> VAX
# SUN -> SUN
#
	if ( ( $nHash =~ /^IEEEIEEE$/ ) ||
	     ( $nHash =~ /^VAXVAX$/ ) ||
	     ( $nHash =~ /^SUNSUN$/ ) )
	{
	}
#
# VAX -> IEEE
#
	elsif ( $nHash =~ /^VAXIEEE$/ )
	{
		if ( ( $aBytes[1] == 0 ) ||
		     ( $aBytes[1] == -128 ) )
		{
			$aBytes[0] = 0;
			$aBytes[1] = 0;
			$aBytes[2] = 0;
			$aBytes[3] = 0;
		}
		else
		{
			$szByte = $aBytes[0];
			$aBytes[0] = $aBytes[2];
			$aBytes[2] = $szByte;
			$szByte = $aBytes[1];
			$aBytes[1] = $aBytes[3];
			$aBytes[3] = $szByte - 1;
		}
	}
#
# IEEE -> VAX
#
	elsif ( $nHash =~ /^IEEEVAX$/ )
	{
		if ( ( ( $aBytes[3] == 0 ) ||
		       ( $aBytes[3] == -128 ) ) &&
                     ( $aBytes[2] >= 0 ) )
		{
			$aBytes[0] = 0;
			$aBytes[1] = 0;
			$aBytes[2] = 0;
			$aBytes[3] = 0;
		}
		elsif ( ( $aBytes[3] == 127 ) ||
                        ( $aBytes[3] == -1 ) )
		{
			$szByte = $aBytes[3];
			$aBytes[0] = 255;
			$aBytes[1] = $szByte;
			$aBytes[2] = 255;
			$aBytes[3] = 255;
		}
		else
		{
			$szByte = $aBytes[0];
			$aBytes[0] = $aBytes[2];
			$aBytes[2] = $szByte;
			$szByte = $aBytes[3];
			$aBytes[3] = $aBytes[1];
			$aBytes[1] = $szByte - 1;
		}
	}
#
# VAX -> SUN
#
	elsif ( $nHash =~ /^VAXSUN$/ )
	{
		if ( ( $aBytes[1] == 0 ) ||
		     ( $aBytes[1] == -128 ) )
		{
			$aBytes[0] = 0;
			$aBytes[1] = 0;
			$aBytes[2] = 0;
			$aBytes[3] = 0;
		}
		else
		{
			$szByte = $aBytes[0];
			$aBytes[0] = $aBytes[1] - 1;
			$aBytes[1] = $szByte;
			$szByte = $aBytes[3];
			$aBytes[3] = $aBytes[2];
			$aBytes[2] = $szByte;
		}
	}
#
# SUN -> VAX
#
	elsif ( $nHash =~ /^SUNVAX$/ )
	{
		if ( ( ( $aBytes[0] == 0 ) ||
		       ( $aBytes[0] == -128 ) ) &&
                     ( $aBytes[1] >= 0 ) )
		{
			$aBytes[0] = 0;
			$aBytes[1] = 0;
			$aBytes[2] = 0;
			$aBytes[3] = 0;
		}
		elsif ( ( $aBytes[0] == 127 ) ||
                        ( $aBytes[0] == -1 ) )
		{
			$szByte = $aBytes[0];
			$aBytes[0] = 255;
			$aBytes[1] = $szByte;
			$aBytes[2] = 255;
			$aBytes[3] = 255;
		}
		else
		{
			$szByte = $aBytes[1];
			$aBytes[1] = $aBytes[0] + 1;
			$aBytes[0] = $szByte;
			$szByte = $aBytes[3];
			$aBytes[3] = $aBytes[2];
			$aBytes[2] = $szByte;
		}
	}
#
# IEEE -> SUN
# SUN -> IEEE
#
	elsif ( ( $nHash =~ /^IEEESUN$/ ) ||
		( $nHash =~ /^SUNIEEE$/ ) )
	{
		$szByte = $aBytes[0];
		$aBytes[0] = $aBytes[3];
		$aBytes[3] = $szByte;
		$szByte = $aBytes[1];
		$aBytes[1] = $aBytes[2];
		$aBytes[2] = $szByte;
	}
	else
	{
		$aBytes[0] = 0;
		$aBytes[1] = 0;
		$aBytes[2] = 0;
		$aBytes[3] = 0;
	}

	$rConverted = unpack("f1",pack("c4",@aBytes));

	return($rConverted);
}

########################################################################
#
#	End of Package
#
########################################################################
1;
