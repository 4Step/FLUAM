/***************************************************************************
 *
 * Authors:		Stephen Lawe, Resource Systems Group, slawe@rsginc.com
 *
 * Purpose:		Land use allocation for Florida Statewide Model.
 *
 * History:		November, 1994 - LUAM conceptual framework first developed
 *
 *				April, 2006 = updated for Florida statewide model. Very 
 *				different from previous LUAM but uses several of the 
 *				basic ITM functionality.
 *
 *				June, 2007 = Version 2.0 of Florida statewide model which is
 *				estimated from parcel level data.
 *
 *         		See 'WinMain.cpp' for extened history
 *
 ***************************************************************************/


#include <windows.h>  
#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "resource.h"                   

#include "winmain.hpp"
#include "landuse.hpp"
#include "fileio.hpp"
#include "readfile.hpp"
#include "par.hpp" 
#include "matrix.hpp"
#include "zon.hpp"
#include "tty.hpp"
#include "lux.hpp"
#include "gro.hpp"

#include "taz.hpp"

using namespace std;


extern cLanduse		oLanduse;

#define BUFSIZE 1024

#define HIGHTEST_TIME 99999.0


// ********************************************************
// *
// * Name:		LUInputsDlgProc
// *
// * Purpose:	dialog box proceedure
// *
// ********************************************************
BOOL CALLBACK LUAMdialog (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
	static cLanduse		tLanduse;

	switch (msg)
	{
		case WM_INITDIALOG:
		{
			tLanduse.doLUAM = false;

			tLanduse.copyLUAM(oLanduse);

			CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER));

			SetDlgItemText (hDlg, IDC_CTLNAME, (LPSTR) tLanduse.CTLName);

			return TRUE;
		}
		case WM_COMMAND:
		{	
			switch (wParam)
			{   
				case IDC_CTL:
				{
					if( GetInputFileName("LUAM Cntrol File", "TXT", tLanduse.CTLName, hDlg) )
						SetDlgItemText (hDlg, IDC_CTLNAME, (LPSTR) tLanduse.CTLName);

					return TRUE;
				}
			
				case IDOK:
				{
					oLanduse.copyLUAM(tLanduse);

					oLanduse.doLUAM = true;

					EndDialog (hDlg, TRUE);

					return TRUE;
				}

				case IDCANCEL:
				{
					oLanduse.doLUAM = false;

					EndDialog (hDlg, FALSE);
				
					return TRUE;
				}
			}
		}
	}

	return FALSE;
}

// ********************************************************
// *
// * Name:		copyIn
// *
// * Purpose:	copy one land use class into another
// *
// ********************************************************
void cLanduse::copyLUAM(cLanduse &s)
{
	doLUAM = s.doLUAM;

	strcpy(CTLName, s.CTLName);

	return;
}

// *********************************************************************************
// *
// * name:		writePrefs
// *
// * purpose:	write the preferences
// *
// *********************************************************************************
BOOL cLanduse::writePrefs( ostream &out )
{
	out << endl << "LANDUSE:"	<< endl;

	writeNameToFile( out, CTLName, "NO_CTL");

	out << "END_LANDUSE"	<< endl;

	if(!out.good()){
		MessageBox (hMainWnd, "Writing to preference file (landuse)", "Error",	MB_ICONSTOP);
		return FALSE;
    }

	return TRUE;
}

// *********************************************************************************
// *
// * name:		readPrefs
// *
// * purpose:	read the preferences
// *
// *********************************************************************************
BOOL cLanduse::readPrefs( istream &in )
{
	char name[128];

	in.getline(name, NAMESIZE);	
	in.getline(name, NAMESIZE);	
	
	if( strcmp(name, "LANDUSE:") ){
		MessageBox (hMainWnd, "Unexpected entry in preference file (landuse).", "Error",	MB_ICONSTOP);
		return FALSE;
    }

	readNameFromFile( in, CTLName);

	if(!in.good()){
		MessageBox (hMainWnd, "Read preference file (landuse)", "Error",	MB_ICONSTOP);
		return FALSE;
    }

	in.getline(name, NAMESIZE);	
	
	if( strcmp(name, "END_LANDUSE") ){
		MessageBox (hMainWnd, "Unexpected end in preference file (landuse).", "Error",	MB_ICONSTOP);
		return FALSE;
    }

	return TRUE;
}

// ********************************************************
// *
// * Name:		readParameters
// *
// * Purpose:	general function for reading the parameter
// *			file. This hands control off to other functions
// *			to read the specifics.
// *
// *			The parameter file contains general model
// *			parameters along with input and output files.
// *			The values are read into the cLandUse members.
// *
// ********************************************************
BOOL cLanduse::readParameters(char *CTLName)
{
	char	sBuf[1024];
	FILE	*fctl = NULL;

	BOOL	verCTL = FALSE;			//  ** VERSION
	BOOL	genCTL = FALSE;			//  ** GENERAL PARAMETERS
	BOOL	frtCTL = FALSE;			//  ** FRATAR REGRESSION PARAMETERS
	BOOL	rcmCTL = FALSE;			//  ** RESIDENTIAL LAND CONSUMPTION COEFFICIENTS
	BOOL	ncmCTL = FALSE;			//  ** NON-RESIDENTIAL LAND CONSUMPTION COEFFICIENTS
	BOOL	rdnCTL = FALSE;			//  ** RESIDENTIAL DENSITY COEFFICIENTS
	BOOL	ndnCTL = FALSE;			//  ** NON-RESIDENTIAL DENSITY COEFFICIENTS
	BOOL	filCTL = FALSE;			//  ** FILE NAMES


	// open the file and do data validation
	if( (fctl = fOpenFile("TXT", CTLName, "r")) == NULL)
		return FALSE;


	// read the file
	while (fgets(sBuf, sizeof(sBuf), fctl) != NULL){

		// if no data in the string, read th enext line  
		if(strlen(sBuf) <= 1)
			continue;

		// if its not one of the headers, read the next line
		if( strstr(sBuf, "**") == NULL)
			continue;


		if( strstr(sBuf, "** VERSION") != NULL){
			verCTL = TRUE;

			if(!readVer(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** GENERAL PARAMETERS") != NULL){
			genCTL = TRUE;

			if(!readGen(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** FRATAR REGRESSION PARAMETERS") != NULL){
			frtCTL = TRUE;

			if(!readFrat(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** RESIDENTIAL LAND CONSUMPTION COEFFICIENTS") != NULL){
			rcmCTL = TRUE;

			if(!readResConsumption(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** NON-RESIDENTIAL LAND CONSUMPTION COEFFICIENTS") != NULL){
			ncmCTL = TRUE;

			if(!readNonResConsumption(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** RESIDENTIAL DENSITY COEFFICIENTS") != NULL){
			rdnCTL = TRUE;

			if(!readResDensity(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** NON-RESIDENTIAL DENSITY COEFFICIENTS") != NULL){
			ndnCTL = TRUE;

			if(!readNonResDensity(fctl))
				return FALSE;

			continue;
		}

		if( strstr(sBuf, "** FILE NAMES") != NULL){
			filCTL = TRUE;
			
			if(!readFileNames(fctl))
				return FALSE;

			continue;
		}
	}

	fclose(fctl);
			
	if(!verCTL){
		MessageBox (hMainWnd, "Did not find '** VERSION' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!genCTL){
		MessageBox (hMainWnd, "Did not find '** GENERAL PARAMETERS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!frtCTL){
		MessageBox (hMainWnd, "Did not find '** FRATAR REGRESSION PARAMETERS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!rcmCTL){
		MessageBox (hMainWnd, "Did not find '** RESIDENTIAL LAND CONSUMPTION COEFFICIENTS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!ncmCTL){
		MessageBox (hMainWnd, "Did not find '** NON-RESIDENTIAL LAND CONSUMPTION COEFFICIENTS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!rdnCTL){
		MessageBox (hMainWnd, "Did not find '** RESIDENTIAL DENSITY COEFFICIENTS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!ndnCTL){
		MessageBox (hMainWnd, "Did not find '** NON-RESIDENTIAL DENSITY COEFFICIENTS' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	if(!filCTL){
		MessageBox (hMainWnd, "Did not find '** FILE NAMES' in CTL file.", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}	

	return TRUE;
}

// *********************************************************************************
// *
// * name:		readVer
// *
// * Sample:
// *
// *		//** VERSION
// *		2010		// version 2.0.1.0 corresponding to the File::About menu
// *
// *********************************************************************************
BOOL cLanduse::readVer(FILE *fctl)
{
	int Version = 2010;
	int nVersion;
	
	if(!getInt(fctl, "readGen", "nVersion", nVersion))
		return FALSE;

	if(nVersion != Version) {
		MessageBox (hMainWnd, "Control file version number incompatible with the application. Current Application Version (2010).", "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}

	return TRUE;
}

// *********************************************************************************
// *
// * name:		readGen
// *
// * Sample:
// *
// *		//** GENERAL PARAMETERS
// *		0			// 1 = dump debug files, 0 = don't
// *		1			// 1 = new skim so reclaculate accessibilities, 0 = no new skim 
// *		1			// 1 = new network so calculate distance to intersections, 0 = no new network
// *		3948		// LastInternalZone
// *		4611		// LastExternalZone
// *		100			// ClosestNTAZs
// *		-1.7354		// TravelTimeAdjustment
// *
// *********************************************************************************
BOOL cLanduse::readGen(FILE *fctl)
{
	if(!getInt(fctl, "readGen", "cfgDebug", cfgDebug))
		return FALSE;

	if(!getInt(fctl, "readGen", "cfgNewSkim", cfgNewSkim))
		return FALSE;

	if(!getInt(fctl, "readGen", "cfgNewNetwork", cfgNewNetwork))
		return FALSE;

	if(!getInt(fctl, "readGen", "LastInternalZone", LastInternalZone))
		return FALSE;

	if(!getInt(fctl, "readGen", "LastExternalZone", LastExternalZone))
		return FALSE;

	if(!getInt(fctl, "readGen", "ClosestNTAZs", ClosestNTAZs))
		return FALSE;

	if(!getFloat(fctl, "readGen", "TravelTimeAdjustment", TravelTimeAdjustment))
		return FALSE;

	return TRUE;
}

// *********************************************************************************
// *
// * name:		readFrat
// *
// * Sample:
// *
// *		//** FRATAR REGRESSION PARAMETERS
// *		1147.784	// Constant
// *		3.61		// RUREMP (Area Type 1)
// *		3.024		// URBEMP (Area Type 2)
// *		3.952		// TRANEMP (Area Type 3)
// *		1.884		// WGHT_HH
// *		1611.567	// Urban area
// *
// *********************************************************************************
BOOL cLanduse::readFrat(FILE *fctl)
{
	if(!getFloat(fctl, "readFrat", "fratConstant", fratConstant))
		return FALSE;

	if(!getFloat(fctl, "readFrat", "fratEmpFact_1", fratEmpFact[0]))
		return FALSE;

	if(!getFloat(fctl, "readFrat", "fratEmpFact_2", fratEmpFact[1]))
		return FALSE;

	if(!getFloat(fctl, "readFrat", "fratEmpFact_3", fratEmpFact[2]))
		return FALSE;

	if(!getFloat(fctl, "readFrat", "fratHHFact", fratHHFact))
		return FALSE;

	if(!getFloat(fctl, "readFrat", "fratUrbanArea", fratUrbanArea))
		return FALSE;

	return TRUE;
}


// *********************************************************************************
// *
// * name:		readResConsumption
// *
// * Sample:
// *
// *		//** RESIDENTIAL LAND CONSUMPTION COEFFICIENTS
// *		-0.70		// constant
// *		4.00		// inside growth boundry
// *		2.40		// change in accessibility
// *		-0.20		// non residentail use
// *		0.18		// coastal TAZ
// *		-0.75		// non residential use for coastal TAZs
// *		1.23		// development
// *		-0.77		// less than 1 acre of vacant
// *		-0.97		// more than 1000 acres of vacant
// *		0.00		// Decile 1: accassibility category
// *		0.02		// Decile 2: accassibility category
// *		0.70		// Decile 3: accassibility category
// *		0.65		// Decile 4: accassibility category
// *		0.88		// Decile 5: accassibility category
// *		1.03		// Decile 6: accassibility category
// *		1.06		// Decile 7: accassibility category
// *		1.10		// Decile 8: accassibility category
// *		1.10		// Decile 9: accassibility category
// *		1.10		// Decile 10: accassibility category
// *
// *********************************************************************************
BOOL cLanduse::readResConsumption(FILE *fctl)
{
	char	buf[128];
	int		i;

	if(!getFloat(fctl, "readResConsumption", "resConstantCoef", resConstantCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resGrowBoundaryCoef", resGrowBoundaryCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resDistToCoast", resDistToCoast))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resAccessChangeCoef", resAccessChangeCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resNonResUseCoef", resNonResUseCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resCoastalTAZCoef", resCoastalTAZCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resNonResCoastalCoef", resNonResCoastalCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resDevelopmentCoef", resDevelopmentCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resSmallVacantCoef", resSmallVacantCoef))
		return FALSE;

	if(!getFloat(fctl, "readResConsumption", "resLargeVacantCoef", resLargeVacantCoef))
		return FALSE;


	resAccessCoef[0] = 0;

	for(i = 1; i <= 10; i++) {
		sprintf(buf, "resAccessCoef%d", i+1);

		if(!getFloat(fctl, "readResConsumption", buf, resAccessCoef[i]))
			return FALSE;
	}
	
	return TRUE;
}

// *********************************************************************************
// *
// * name:		readNonResConsumption
// *
// * Sample:
// *
// *		//** NON-RESIDENTIAL LAND CONSUMPTION COEFFICIENTS
// *		-1.88		// constant
// *		0.55		// inside growth boundry
// *		-0.24		// distance to ramp
// *		-1.77		// change in accessibility
// *		0.00		// Decile 1: accassibility category
// *		0.21		// Decile 2: accassibility category
// *		0.27		// Decile 3: accassibility category
// *		0.32		// Decile 4: accassibility category
// *		0.38		// Decile 5: accassibility category
// *		0.45		// Decile 6: accassibility category
// *		0.56		// Decile 7: accassibility category
// *		0.65		// Decile 8: accassibility category
// *		0.96		// Decile 9: accassibility category
// *		1.07		// Decile 10: accassibility category
// *
// *********************************************************************************
BOOL cLanduse::readNonResConsumption(FILE *fctl)
{
	char	buf[128];
	int		i;

	if(!getFloat(fctl, "readNonResConsumption", "nonresConstantCoef", nonresConstantCoef))
		return FALSE;

	if(!getFloat(fctl, "readNonResConsumption", "nonresGrowBoundaryCoef", nonresGrowBoundaryCoef))
		return FALSE;

	if(!getFloat(fctl, "readNonResConsumption", "nonresDistToRampCoef", nonresDistToRampCoef))
		return FALSE;

	if(!getFloat(fctl, "readNonResConsumption", "nonresAccessChangeCoef", nonresAccessChangeCoef))
		return FALSE;


	nonresAccessCoeff[0] = 0;

	for(i = 1; i <= 10; i++) {
		sprintf(buf, "nonresAccessCoeff%d", i+1);

		if(!getFloat(fctl, "readNonResConsumption", buf, nonresAccessCoeff[i]))
			return FALSE;
	}
	
	return TRUE;
}

// *********************************************************************************
// *
// * name:		readResDensity
// *
// * Sample:
// *
// *		//** RESIDENTIAL DENSITY COEFFICIENTS
// *		0.15		// constant
// *		0.46		// developed density
// *		0.58		// change in accessibility
// *		12.11		// accessibility
// *		-1.24		// less than 1 acre of vacant
// *		-0.39		// more than 1000 acres of vacatn
// *		0.00		// Decile 1: accassibility category
// *		0.13		// Decile 2: accassibility category
// *		0.23		// Decile 3: accassibility category
// *		0.24		// Decile 4: accassibility category
// *		0.23		// Decile 5: accassibility category
// *		0.31		// Decile 6: accassibility category
// *		0.26		// Decile 7: accassibility category
// *		0.21		// Decile 8: accassibility category
// *		0.40		// Decile 9: accassibility category
// *		0.60		// Decile 10: accassibility category
// *
// *********************************************************************************
BOOL cLanduse::readResDensity(FILE *fctl)
{
	char	buf[128];
	int		i;

	if(!getFloat(fctl, "readResDensity", "d_resConstantCoef", d_resConstantCoef))
		return FALSE;

	if(!getFloat(fctl, "readResDensity", "d_resDevelopmentDensCoef", d_resDevelopmentDensCoef))
		return FALSE;

	if(!getFloat(fctl, "readResDensity", "d_resAccessChangeCoef", d_resAccessChangeCoef))
		return FALSE;

	if(!getFloat(fctl, "readResDensity", "d_resSmallVacantCoef", d_resSmallVacantCoef))
		return FALSE;

	if(!getFloat(fctl, "readResDensity", "d_resLargeVacantCoef", d_resLargeVacantCoef))
		return FALSE;

	d_resAccessCoef[0] = 0;

	for(i = 1; i <= 10; i++) {
		sprintf(buf, "d_resAccessCoef%d", i+1);

		if(!getFloat(fctl, "readResDensity", buf, d_resAccessCoef[i]))
			return FALSE;
	}
	
	return TRUE;
}

// *********************************************************************************
// *
// * name:		readNonResDensity
// *
// * Sample:
// *
// *		//** NON-RESIDENTIAL DENSITY COEFFICIENTS
// *		-15.04		// constant
// *		12.11		// accessibility
// *
// *********************************************************************************
BOOL cLanduse::readNonResDensity(FILE *fctl)
{
	if(!getFloat(fctl, "readNonResDensity", "d_nonresConstantCoef", d_nonresConstantCoef))
		return FALSE;

	if(!getFloat(fctl, "readNonResDensity", "d_nonresAccessCoef", d_nonresAccessCoef))
		return FALSE;

	return TRUE;
}

// *********************************************************************************
// *
// * name:		readGenPar
// *
// * Sample:
// *
// *		//** FILE NAMES
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\TTSkimForSteve.txt		// INPUT: travel time skim
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_TAZ.txt				// INPUT: zone input file for LUAM
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_Growth.txt			// INPUT: growth file
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_Nodes.txt				// INPUT: node file
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_Links.txt				// INPUT: link file
// *		C:\Projects\FDOT LUAM\For W\DensityConstraints.txt 				// INPUT: density constraints file
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_TAZ_Out.txt			// OUTPUT: zonal output file
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_TAZ_Next_In.txt		// OUTPUT: zonal output file to be used as next input TAZ file
// *		Q:\WGX\02047 - FloridaTpk\LUAM_ITM\Data\d_FRATAR_Input.txt		// OUTPUT: input to fratar process
// *
// *********************************************************************************
BOOL cLanduse::readFileNames(FILE *fctl)
{

	if(!getString(fctl, "readFileNames", "fNameTTY", fNameTTY))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fNameTAZ", fNameTAZ))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fNameGrowth", fNameGrowth))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fNameNode", fNameNode))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fNameLink", fNameLink))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fNameDensityConstraints", fNameDensityConstraints))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fOutName", fOutName))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fOutNameNextTAZ", fOutNameNextTAZ))
		return FALSE;

	if(!getString(fctl, "readFileNames", "fOutNameFratar", fOutNameFratar))
		return FALSE;


	return TRUE;
}

// *********************************************************************************
// *
// * name:		getInt
// *
// * purpose:	read an int from the PAR file. Have to dump comments, etc.
// *
// *********************************************************************************
BOOL cLanduse::getInt(FILE *fctl, char *fnct, char *data, int &saveIt)
{
	float temp;

	if(!getFloat(fctl, fnct, data, temp))
		return FALSE;

	saveIt = (int)temp;

	return TRUE;
}
// *********************************************************************************
// *
// * name:		getFloat
// *
// * purpose:	read a float from the PAR file. Have to dump comments, etc.
// *
// *********************************************************************************
BOOL cLanduse::getFloat(FILE *fctl, char *fnct, char *data, float &saveIt)
{
	char	sBuf[BUFSIZE];

	if(!getString(fctl, fnct, data, sBuf))
		return FALSE;

	if(1 != sscanf(sBuf, "%f", &saveIt)){
		sprintf(sBuf, "Not able to extract data from CTL in '%s' (%s).", fnct, data);
		MessageBox (hMainWnd, sBuf, "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}
	
	return TRUE;	
}

// *********************************************************************************
// *
// * name:		getString
// *
// * purpose:	read a string from the file and dump comments.
// *		
// * Params:	FILE *fpar	= the file to read from
// *			char *fnct  = the calling function - used during error msg
// *			char *data  = the piece of information being read
// *			char *sBuf	= where the output is stored
// *
// *********************************************************************************
BOOL cLanduse::getString(FILE *fctl, char *fnct, char *data, char *sBuf)
{

	// get the line
	if(fgets(sBuf, BUFSIZE, fctl) == NULL){
		sprintf(sBuf, "File read failed on '%s' (%s).", fnct, data);
		MessageBox (hMainWnd, sBuf, "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}

	// is long enough
	if( strlen(sBuf) <= 1){
		sprintf(sBuf, "Unexpected line length in PAR on '%s' (%s).", fnct, data);
		MessageBox (hMainWnd, sBuf, "Error", MB_ICONSTOP | MB_OK);
		return FALSE;
	}

	stripComments(sBuf);

	return TRUE;
}

// *********************************************************************************
// *
// * name:		ReadGrowthFile
// *
// * purpose:	This file is an input file containing the growth increments by "growth region"
// *			The growth region could be a GrowthCenter, town, or any other geographic area. TAZ's
// *			are aggregated into growth regions in the first column of the TAZ file. This is
// *			called GrowthCenter becuase we expect it to be used that way most of the time.
// *
// * file structure:
// *
// *			File length:
// *				number of growth regions + header
// *
// *			Header:
// *				[0] = number of growth regions
// *				[1] = "housing" text header
// *				[2] = "employment" text header
// *
// *			The Body of the file contains:
// *
// *				[0] = growth region number (GrowthCenter number most of the time)
// *				[1] = housing increment for the specified growth region
// *				[2] = employment increment for the specified growth region
// *
// * rules:		Each growth region must have one and only one record.
// *			File must be in order from growth center 1 ... n
// *
// *********************************************************************************
int cLanduse::readGrowthFile(char *fNameGrowth, cMatrix &oGrowthIncrement, cTAZ &oTAZ)
{
	int		i, nGrowthCenters, GrowthCenter;
	float	housing, employment;
	char	buf[256];

	FILE	*f;

	// open the land use file for reading
	if( (f = fOpenFile("TXT", fNameGrowth, "r")) == NULL)
		return 0;

	// read header
	fgets(buf, sizeof(buf), f);

	if( 1 != sscanf(buf, "%d", &nGrowthCenters)) {
		MessageBox (hMainWnd, "failed reading header of growth file.", "Error",	MB_ICONSTOP);
		fclose(f);
		return 0;
	}

	if(nGrowthCenters != oTAZ.nGrowthCenter) {
		sprintf(buf, "Found %d growth region(s) in header of growth file but the largest growth regions in TAZ file was %.0f", nGrowthCenters, oTAZ.nGrowthCenter);
		MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
		fclose(f);
		return 0;
	}

	// allocate memory [one-based nGrowthCenters][housing or employment]
	oGrowthIncrement.AllocateMatrix(nGrowthCenters + 1, 2);

	
	// read the growth increments
	for(i = 0; i < nGrowthCenters; i++) {

		if( 3 != fscanf(f, "%d, %f, %f", &GrowthCenter, &housing, &employment)) {
			sprintf(buf, "Failed to read row %d of the growth file.", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}

		if( GrowthCenter != i+1 ) {
			sprintf(buf, "Found a growth region out of order in row %d of the growth file.", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}

		if(NULL == fgets(buf, sizeof(buf), f)) {
			sprintf(buf, "Failed to read row %d of the growth file (expected a name in the last column.", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}

		oGrowthIncrement.mat[i+1][0] = housing;
		oGrowthIncrement.mat[i+1][1] = employment;
	}

	fclose(f);

	return 1;
}

// *********************************************************************************
// *
// * name:		readDensityConstraints
// *
// * purpose:	This file is an input file containing the housing and employment density constraints. 
// *			When the allocation is calculated the constraint is used as a maximum limit
// *
// * file structure:
// *
// *			File length:
// *				number of TAZs
// *
// *			The Body of the file contains:
// *
// *				TAZ,Res Density,Non-Res Density,County,Development Name,Land Use Description,,,,,
// *
// *				[0] = TAZ
// *				[1] = housing constraint
// *				[2] = employment constraint
// *				...
// *
// * rules:		File must be in order by TAZ 1 ... n
// *
// *********************************************************************************
int cLanduse::readDensityConstraints(char *fNameDensityConstraints, cTAZ &oTAZ)
{
	int		i, TAZ;
	float	housingConstraint, employmentConstraint;
	char	buf[256];

	FILE	*f;

	// open the file for reading
	if( (f = fOpenFile("TXT", fNameDensityConstraints, "r")) == NULL)
		return 0;


	// skip first row	//	TAZ,Res Density,Non-Res Density,County,Development Name,Land Use Description,,,,,
	if (NULL == fgets(buf, sizeof(buf), f)){
		sprintf(buf, "Failed reading first row of the Density Constraints file.");
		MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
		fclose(f);
		return 0;
	}

	
	// read the growth increments
	for(i = 0; i < LastInternalZone; i++) {

		if(NULL == fgets(buf, sizeof(buf), f)) {
			sprintf(buf, "Failed to read TAZ %d of the Density Constraints file (unexpected end).", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}

		if( 3 != sscanf(buf, " %d, %f, %f", &TAZ, &housingConstraint, &employmentConstraint)) {
			sprintf(buf, "Failed reading Density Constraints file in line %ld.", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}


		if( TAZ != i+1 ) {
			sprintf(buf, "Found a TAZ out of order in row %d of the Density Constraints file.", i+1);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(f);
			return 0;
		}


		oTAZ.Recs[i].housingDensityConstraint = housingConstraint;
		oTAZ.Recs[i].employmentDensityConstraint = employmentConstraint;
	}

	fclose(f);

	return 1;
}

// ********************************************************
// *
// * The following is the core of the florida land use model
// *
// ********************************************************



// ********************************************************
// *
// * Name:		RunFloridaLandUse
// *
// * Purpose:	copy input files from one object to another
// *
/*

process:

1.
read the parameter file which contains the general parameters
for model operation and the input and output file names to be
used for the model run.

2.
read the TAZ file which contains the inputs, intermediate
outputs and final outputs for for the model run. The inputs
include taz, growth zone, district, housing, employment, ...

3
read the growth file which contains the control total for each
growth center.

4.
calculate the weighted travel times for each of the taz's. This
is the travel times from each origin TAZ to the closest (n) TAZs
where (n) is defined in the parameter file. These closest travel
times are then converted so that large numbers are better using
a TT^x where x is defined in the parameter file. Finally, the converted
travel times are weighted by (multiplied by) the housing and employment
of each of the destination TAZs for the closest (n). The results of
these calculations are divided by (n) to produce the average.


*/
// ********************************************************
int cLanduse::RunFloridaLandUse(void)
{
	cMatrix oGrowthIncrement;

	int		fConverged;
	int		iters = 0;


	header("Running Florida LUAM");


	// important to read this file first because it contains the
	// number of zones, etc which are used below to dimension arrays
	if( !readParameters(CTLName) )
		return 0;


	// a class that stores the inputs and outputs for each taz
	cTAZ oTAZ(LastInternalZone);


	// a class that stores the inputs and outputs for each taz
	cTAZ oTAZnew(LastInternalZone);

	
	// read the land use into the taz matrix which contains
	// land use, intermediate results, and final results
	if( ! oTAZ.readFile(fNameTAZ) )
		return 0;


	// read the growth file matrix which has control totals by growth district
	if( !readGrowthFile(fNameGrowth, oGrowthIncrement, oTAZ) )
		return 0;	

	// read the density constraints used for housing and employment calcs
	if( !readDensityConstraints(fNameDensityConstraints, oTAZ) )
		return 0;

	// * calculate the access variable *
	if( this->cfgNewSkim ) {

		// calculates the weighted accessibility to the closest (n) tazs
		if( !calculateWeightedTTForAllTAZs(oTAZ) )
			return 0;

		// calculate a travel time for each TAZ weighted by land use
		if( !calculateScaledAccess(oTAZ) )
			return 0;

		// final calculation for accessability - turn into categorical 1-10
		if( !calculateCategoricalAccess(oTAZ) )
			return 0;

	}


	// * calculate the distance to arterial and ramp nodes *
	if( this->cfgNewNetwork ) {

		if( !calculateDistToIntersections(oTAZ) )
			return 0;
	}



	// calculate the land consumption
	if( ! CalculateLandConsumption(oTAZ) )
		return 0;


	// generate new housing
	do {
		fConverged = 0;
		iters += 1;


		// calculate the final values
		// return vals {0 = failure, 1 = converged, -100 not converged}
		if( ! (fConverged += CalculateNewHousing(oTAZ, oGrowthIncrement ) ) )
			return 0;


	} while (fConverged < 0 && iters < 20);


	iters = 0;

	// generate new employment
	do {
		fConverged = 0;
		iters += 1;


		// calculate the final values
		if( ! (fConverged += CalculateNewEmployment(oTAZ, oGrowthIncrement ) ) )
			return 0;


	} while (fConverged < 0 && iters < 20);


	// now write the result to a file
	if( ! oTAZ.writeFile(fOutName) )
		return 0;


	// calculate the final values
	if( ! oTAZnew.CreateNewTAZFile(oTAZ ) )
		return 0;


	// now write the result to a file
	if( ! oTAZnew.writeFile(fOutNameNextTAZ) )
		return 0;


	// calculate and write the file which will be used for FRATAR input later
	if( ! CalculateFratarOutput(oTAZ) )
		return 0;


	header("Done");

	return 1;
}
	
// ********************************************************
// *
// * Name:		calculateWeightedTT
// *
// * Purpose:	Calculate a travel time for each TAZ weighted by land use
// *
// * Process:	Opens the travel time matrix and, for each
// *			origin TAZ (row), create a list of the closest (n)
// *			TAZs and their travel times. Once this list is
// *			created and all destination TAZs have been visited,
// *			calculate a weighted travel time for the origin TAZ
// *
// ********************************************************
int cLanduse::calculateWeightedTTForAllTAZs(cTAZ &oTAZ)
{
	int		i;
	int		currOrig = 1;
	int		orig = 0;
	int		dest = 0;
	float	time = 0.0;
	long	recs = 0;
	char	buf[256];

	FILE	*ftty;
	FILE	*fcloseTAZ;
	FILE	*fcloseTTY;

	// check that each TAZ is listed in the file
	// consistency check with the number of internal and external taz's	
	cArray<int> ttyCheck(LastExternalZone + 1);
	
	// two arrays that store the closest (n) TAZs
	cArray<int> tempAccessTAZ(ClosestNTAZs + 1);

	
	// and the travel time to those TAZs
	cArray<float> tempAccess(ClosestNTAZs + 1);


	// open the travel time file for reading
	if( (ftty = fOpenFile("TXT", fNameTTY, "r")) == NULL)
		return 0;


	if(this->cfgDebug) {	

		// make a file name for the TAZ output file
		if( !MakeFileName(fOutName, ".TAZ", buf) )
			return 0;

		// open an output file for dumping the closest TAZs
		if( (fcloseTAZ = fOpenFile("TXT", buf, "w")) == NULL)
			return 0;

		// make a file name for the travel time output file
		if( !MakeFileName(fOutName, ".TIM", buf) )
			return 0;

		// open an output file for dumping the closest travel times
		if( (fcloseTTY = fOpenFile("TXT", buf, "w")) == NULL)
			return 0;
	}

	// setup array for next use
	for(i = 0; i < tempAccess.nRows; i++)
		tempAccess.arr[i] = HIGHTEST_TIME;


	// read the entire travel time file
	while (NULL != fgets(buf, sizeof(buf), ftty)) {

		// some records have orig, dest, and then a blank. Skip these records
		if( 3 != sscanf(buf, "%d, %d, %f", &orig, &dest, &time))
			time = HIGHTEST_TIME + 1;


		// make sure the origin and destination are less than or equal to the max zones they gave us
		if( orig > LastExternalZone || dest > LastExternalZone ) {
			sprintf(buf, "Either origin (%d) or destination (%d) in the skim file is greater than the max TAZ specified in the control file (%d).", orig, dest, LastExternalZone);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			return 0;
		}

		// this taz has been visited
		ttyCheck.arr[orig] = 1;

		// we are starting a new orig TAZ so calculate the  
		// weighted travel times for the last orig
		if(currOrig != orig) {
			
			if(this->cfgDebug) {	
				// TRACKING: just as a means of tracking 
				fprintf(fcloseTAZ, "%d,", currOrig);
				fprintf(fcloseTTY, "%d,", currOrig);

				for(i = 1; i < tempAccess.nRows; i++) {
					fprintf(fcloseTTY, "%f,", tempAccess.arr[i]);
					fprintf(fcloseTAZ, "%d,", tempAccessTAZ.arr[i]);
				}

				fprintf(fcloseTAZ, "\n");
				fprintf(fcloseTTY, "\n");
				// END TRACKING
			}

			// calculate weighted access times
			if(! CalculateWeightedTimes(oTAZ, tempAccess, tempAccessTAZ, currOrig)) {
				fclose(ftty);

				if(this->cfgDebug) {	
					fclose(fcloseTAZ);
					fclose(fcloseTTY);				
				}				
					
				return 0;
			}
				
			// setup array for next use
			for(i = 0; i < tempAccess.nRows; i++)
				tempAccess.arr[i] = HIGHTEST_TIME;

			// we are now starting a new taz
			currOrig = orig;
		}


		// if both ends are internal and there is a travel time and
		// the travel time is less than the max currently in the list then
		// insert this value into the array of closest tazs
		if(orig <= LastInternalZone && dest <= LastInternalZone && time > 0.01 && time < tempAccess.arr[ClosestNTAZs] )
			insertIntoArray( tempAccess.arr, tempAccessTAZ.arr, time, dest, ClosestNTAZs);		

		// just for page reporting
		recs++;

		// only do page reporting after every 50,000 records
		if( !((recs+1) % 50000)){
 			sprintf(buf, "ReadFile (rec %d)", recs + 1);
			header(buf);
		}

	}


	// the last TAZ will not be handled because of the loop structure	
	// calculate weighted access times
	if(! CalculateWeightedTimes(oTAZ, tempAccess, tempAccessTAZ, currOrig)) {
		fclose(ftty);

		if(this->cfgDebug) {	
			fclose(fcloseTAZ);
			fclose(fcloseTTY);				
		}		
		
		return 0;
	}

	if(this->cfgDebug) {	
		// TRACKING: just as a means of tracking 
		fprintf(fcloseTAZ, "%d,", currOrig);
		fprintf(fcloseTTY, "%d,", currOrig);

		for(i = 1; i < tempAccess.nRows; i++) {
			fprintf(fcloseTTY, "%f,", tempAccess.arr[i]);
			fprintf(fcloseTAZ, "%d,", tempAccessTAZ.arr[i]);
		}

		fclose(fcloseTAZ);
		fclose(fcloseTTY);	
		// END TRACKING
	}


	// NOTE: only loop through the internal tazs even though the 
	// array covers both internal and external
	for(i = 1; i <= LastInternalZone; i++) {	

		if( ! ttyCheck.arr[i] ) {
			sprintf(buf, "Travel time file did not contain an origin node number %d even though the last external taz was listed as %d.", i, LastExternalZone);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			return 0;
		}

	}

	// now close the files and move on
 	sprintf(buf, "ReadFile (rec %d)", recs);
	header(buf);

	fclose(ftty);

	return 1;
}

// *********************************************************************************
// *
// * Name:		calculateScaledAccess
// *
// * Purpose:	This scales the access within a DOT district
// *
// *********************************************************************************
int cLanduse::calculateScaledAccess(cTAZ &oTAZ)
{
	int		j;
	int		taz;
	int		iDistrict;
	int		nFor99th;
	int		iThe99th;
	float	the99th;
	float	tempNegAccess;
	float	resultOf99th;

	// to speed up the second loop
	cArray<int> TAZsInDistrict(LastInternalZone);

	// dimension it once because this array is never used
	cArray<int> tempAccessTAZ(LastInternalZone);


	// for each district in the model
	for(iDistrict = 1; iDistrict <= oTAZ.nDOTDistrict; iDistrict++) {
		int nInDistrict = 0;


		// loop through all zones and count the number
		// of TAZ's in the district we are working on.
		for(j = 0; j < LastInternalZone; j++) {

			if(oTAZ.Recs[j].DOTDistrict == iDistrict) {
				TAZsInDistrict.arr[nInDistrict] = j;
				nInDistrict++;
			}
		}
			

		// round up to get the 99th percentile
		the99th = nInDistrict - (0.99 * (nInDistrict-1));

		// +1 for 1 based and +1 more just to be safe
		nFor99th = ((int)the99th + 0.5) + 2;

		// array of the smallest (n) accessibilities and its TAZ 
		// in the list of tazs in each DOT district
		cArray<float> tempAccess(nFor99th + 1);


		// seed it so we correctly get lowest values
		for(j = 0; j < nFor99th + 1; j++) 
			tempAccess.arr[j] = HIGHTEST_TIME;

		// now loop through the zones in the district we are 
		// working on. This will result in an ordering of the 
		// top (n) taz's from which we will grab the 99th percentile
		for(j = 0; j < nInDistrict; j++) {
			taz = TAZsInDistrict.arr[j];

			// make this value negative because the current InsertIntoArray
			// functionality supports finding the (n) smallest values
			// also add 3 because we add 3 below in the log equation
			tempNegAccess = -1 * (oTAZ.Recs[taz].avgAdjTTimeWgtByHH_Emp + 3);
				
			// if the travel time is less than the max currently in the list then
			// insert this value into the array of closest tazs
			if(tempNegAccess < tempAccess.arr[nFor99th] )
				insertIntoArray( tempAccess.arr, tempAccessTAZ.arr, tempNegAccess, taz, nFor99th);		

		}

		// establish the lower cell in the array for the 99th percentil
		iThe99th = ((int)the99th);
		
		// if we landed directly on a location then use it
		if(iThe99th == the99th) {

			resultOf99th = -1 * (tempAccess.arr[iThe99th]);
		
		} else {
			float interpolate = the99th - (int)the99th;			

			resultOf99th =	(-1 * (tempAccess.arr[iThe99th + 1] * interpolate)) + 
							(-1 * (tempAccess.arr[iThe99th] * (1-interpolate)));

		}


		// finally, store the values in the TAZ structure
		for(j = 0; j < nInDistrict; j++) {
			taz = TAZsInDistrict.arr[j];
	
			oTAZ.Recs[taz].the99th = resultOf99th;

			// this will happen if there is not travel time to the TAZ
			if(oTAZ.Recs[taz].avgAdjTTimeWgtByHH_Emp > 0)
				oTAZ.Recs[taz].accessScaled = min(1, log(oTAZ.Recs[taz].avgAdjTTimeWgtByHH_Emp + 3) / log(resultOf99th) );	
			else
				oTAZ.Recs[taz].accessScaled = 0;

		}

	}	
	return 1;
}

// *********************************************************************************
// *
// * Name:		CalculateCategoricalAccess
// *
// * Purpose:	
// *
// *********************************************************************************
int cLanduse::calculateCategoricalAccess(cTAZ &oTAZ)
{
	int		i,j,z;
	int		iDistrict;

	// to speed up the second loop
	cArray<int> TAZsInDistrict(LastInternalZone);


	// for each district in the model
	for(iDistrict = 1; iDistrict <= oTAZ.nDOTDistrict; iDistrict++) {
		int nInDistrict = 0;


		// loop through all zones and count the number of TAZ's in the district we are working on.
		for(j = 0; j < LastInternalZone; j++) {

			if(oTAZ.Recs[j].DOTDistrict == iDistrict) {
				TAZsInDistrict.arr[nInDistrict] = j;
				nInDistrict++;
			}
		}

		// now loop through the zones in the district
		for(i = 0; i < nInDistrict; i++) {
			int	  taz = TAZsInDistrict.arr[i];
			float nBelow = 0;
			float nAbove = 0;
			float access = oTAZ.Recs[taz].accessScaled;

			for(j = 0; j < nInDistrict; j++) {
				z = TAZsInDistrict.arr[j];

				if(oTAZ.Recs[z].accessScaled > access)
					nAbove += 1;
				else if(oTAZ.Recs[z].accessScaled < access)
					nBelow += 1;

			}
				
			if(nBelow + nAbove > 0)
				oTAZ.Recs[taz].accessCategorical = min(10, (int) ((nBelow / (nBelow + nAbove)) * 10) + 1);
			else
				oTAZ.Recs[taz].accessCategorical = 0;
		
		}
	
	}


	return 1;
}
// *********************************************************************************
// *
// * findLocationToInsert:
// *
// *	Uses a binary search to identify the correct location to insert the value
// *	[time] into the array [f]. This assumes the array [f] is already sorted 
// *	from smallest to largest (ascending).
// *
// * Arguments:
// *
// *	f = array of closest N taz's by travel time to the TAZ currently being processed
// *	time = the time to be inserted
// *	theMax = the size of the arrays (number of closest TAZ's to remember)
// *
// *********************************************************************************
int cLanduse::findLocationToInsert(float *f, float time, int theMax)
{
	int nMax = theMax;
	int nMin = 1;
	int middle;

	while( (nMax - nMin) > 1) {

		middle = nMin + (nMax - nMin) / 2;

		if( time > f[ middle ] ) {

			nMin = middle + 1;

		} else {

			nMax = middle;

		}

	}

	if( time > f[ nMin ] ) 
		return nMax;

	return nMin;
}

// *********************************************************************************
// *
// * InsertIntoArray:
// *
// *	Uses and insert algorith that finds the location to insert
// *	the new value into the array using a binary search and then simply pushes
// *	all the remaining values out one to make space.
// *
// * Arguments:
// *
// *	f = array of closest N taz's by travel time to the TAZ currently being processed
// *	t = array of the closest taz numbers representing the closest
// *	time = the time to be inserted
// *	taz = the taz corresponding to the time
// *	theMax = the size of the arrays (number of closest TAZ's to remember)
// *
// *********************************************************************************
int cLanduse::insertIntoArray(float *f, int *t, float time, int taz, int theMax)
{
	int i;
	int start = findLocationToInsert(f, time, theMax);
	
	// move everything
	for(i = theMax; i > start; i--) {
		f[i] = f[i-1];
		t[i] = t[i-1];
	}

	// now insert
	f[start] = time;
	t[start] = taz;

	return 1;
}

// *********************************************************************************
// *
// * CalculateWeightedTimes:
// *
// *	Calculate the weighted times for the closest N TAZs and then average them.
// *	Store the results in the oTAZ matrix that will be used later.
// *
// * Process:
// *
// *	Adjust the travel time so that higher numbers are better. This way we can
// *	weight by multiplying employment and housing and the higher values are better.
// *
// *		convTravelTime = time ^ -1.7354	
// *
// * Arguments:
// *
// *	oTAZ = matrix to store the results in
// *	tempAccess = array of closest N taz's by travel time to the TAZ currently being processed
// *	tempAccessTAZ = array of the closest taz numbers representing the closest
// *	currOrig = the origin TAZ that we are working on at the moment
// *
// *********************************************************************************
int cLanduse::CalculateWeightedTimes(cTAZ &oTAZ, cArray<float> &tempAccess, cArray<int> &tempAccessTAZ, int currOrig)
{
	int	  index;
	int	  failed = 0;
	float housing;
	float employment;
	
	float sumTT = 0.0;
	float sumConvTT = 0.0;
	float sumHHWgt = 0.0;
	float sumEMWgt = 0.0;
	float sumHHEMWgt = 0.0;

	// We set nRows to nClosest + 1 earlier but this was to make it 1 based
	float size = tempAccess.nRows;


	// make sure we are not accessing bad memory
	if( size <= 0 ) {
		MessageBox (hMainWnd, "Expected to have a denominator > 0 in CalculateWeightedTimes().", "Error",	MB_ICONSTOP);
		return 0;
	}


	// make sure we are not accessing bad memory
	if( currOrig > oTAZ.nRecords ) {
		MessageBox (hMainWnd, "Orig TAZ in travel time file greater than expected.", "Error",	MB_ICONSTOP);
		return 0;
	}


	// crank through the N closest TAZs
	for(int i = 1; i < tempAccess.nRows; i++) {

		// this means that the currOrig was not accesible to
		// (n) other TAZ's or it is an external taz
		if(tempAccess.arr[i] == HIGHTEST_TIME) {
			failed = 1;
			break;
		}

		// sum the converted travel time
		sumTT += tempAccess.arr[i];

		// convert time
		tempAccess.arr[i] = pow(tempAccess.arr[i], TravelTimeAdjustment);

		// sum the converted travel time
		sumConvTT += tempAccess.arr[i];

		// make sure we are not accessing bad memory
		if( tempAccessTAZ.arr[i] > oTAZ.nRecords ) {
			MessageBox (hMainWnd, "Dest TAZ in travel time file greater than expected.", "Error",	MB_ICONSTOP);
			return 0;
		}

		index = tempAccessTAZ.arr[i] - 1;

		housing = oTAZ.Recs[index].housing;

		employment = oTAZ.Recs[index].employment;

		sumHHWgt += tempAccess.arr[i] * housing;

		sumEMWgt += tempAccess.arr[i] * employment;

		sumHHEMWgt += tempAccess.arr[i] * (housing + employment);
	}


	index = currOrig - 1;

	if(failed == 0) {
		oTAZ.Recs[ index ].avgTTime = sumTT / size;
		oTAZ.Recs[ index ].avgAdjTTime = sumConvTT / size;
		oTAZ.Recs[ index ].avgAdjTTimeWgtByHH = sumHHWgt / size;
		oTAZ.Recs[ index ].avgAdjTTimeWgtByEmp = sumEMWgt / size;
		oTAZ.Recs[ index ].avgAdjTTimeWgtByHH_Emp = sumHHEMWgt / size;
	}

	return 1;
}

// ********************************************************
// *
// * Name:		calculateDistToIntersections
// *
// * Purpose:	calculate the distance from each TAZ to the 
// *			closest arterial and ramp nodes
// *
// ********************************************************
int cLanduse::calculateDistToIntersections(cTAZ &oTAZ)
{
	long		maxNode;
	cMatrix		nodes;

	header("Calculating distance to intersections");

	// get the largest node number to support mem allocation
	if(! (maxNode = getLargestNode()) )
		return 0;

	// is this a ramp node or not
	cArrayLarge<int> rampNode(maxNode + 1);

	// is this an arterial node or not
	cArrayLarge<int> arterialNode(maxNode + 1);
	
	// identify nodes as either ramp or arterial
	if( !getNodeClasses(rampNode, arterialNode) )
		return 0;

	// now get the x and y coordinates for the intersections we care about
	if( !getNodeCoords(nodes, rampNode, arterialNode) )
		return 0;

	// final step, load the oTAZ structure with the results
	if( !calculateDistances(oTAZ, nodes) )
		return 0;


	return 1;
}

// ********************************************************
// *
// * Name:		getLargestNode
// *
// * Purpose:	read the node file and return the largest
// *			node number.
// *
// ********************************************************
int cLanduse::getLargestNode(void)
{
	long	row = 0;
	long	node;
	long	maxNode = 0;
	char	buf[256];

	FILE *fnde;

	// open the file and do data validation
	if( (fnde = fOpenFile("TXT", fNameNode, "r")) == NULL)
		return 0;


	// read header
	fgets(buf, sizeof(buf), fnde);


	// loop through the entire file
	while (NULL != fgets(buf, sizeof(buf), fnde)) {
		row++;

		if( 1 != sscanf(buf, "%d", &node)) {
			sprintf(buf, "Failed reading node file in line %ld.", row);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(fnde);
			return 0;
		}

		maxNode = max(maxNode, node);
	}

	fclose(fnde);

	return maxNode;
}

// ********************************************************
// *
// * Name:		getNodeClasses
// *
// * Purpose:	Read the link file and assess whether the
// *			nodes on either side are ramp or arterial
// *
// ********************************************************
int cLanduse::getNodeClasses(cArrayLarge<int> &rampNode, cArrayLarge<int> &arterialNode)
{
	float	nFrom, nTo, var1, var2, var3, var4, linkType;
	long	row = 0;
	int		iLType;
	long	orig;
	long	dest;
	char	buf[256];

	FILE	*flnx;


	// open the link file
	if( (flnx = fOpenFile("TXT", fNameLink, "r")) == NULL)
		return 0;

	// skip first row		//a,b,district,distance,oneway,lane_05,ftype_05,atype_05,speed_05,cap_05,fftime_05
	if (NULL == fgets(buf, sizeof(buf), flnx))
		return 0;

	// loop through entire file
	while (NULL != fgets(buf, sizeof(buf), flnx)) {
		row++;

		if( 7 != sscanf(buf, " %f, %f, %f, %f, %f, %f, %f", &nFrom, &nTo, &var1, &var2, &var3, &var4, &linkType)) {
			sprintf(buf, "Failed reading link file in line %ld.", row);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(flnx);
			return 0;
		}

		iLType = (int)linkType;
		orig = (long)nFrom;
		dest = (long)nTo;


		if(orig >= arterialNode.nRows || dest >= arterialNode.nRows) {
			sprintf(buf, "Found node in link file larger than the biggest node number in the node file (link: orig=%ld, dest=%ld) (biggest in node file %ld.", orig, dest, arterialNode.nRows);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(flnx);
			return 0;
		}


		// link type logic:
		//
		// arterial logic:
		//		1. node must connect to an arterial link (set to * 100)
		// 
		//		2. must also connect to at least two other links of 
		//		   arterial or above (expressway, ramp, etc.)
		//
		// ramp logic:
		//		1. node must connect to a ramp link. (set to * 100)
		// 
		//		2. node must then connect to a link of any type other
		//		   than highways & tolls because there are links from
		//		   highway to highway that are only accessible if you  
		//		   are already on the highway
		//		

		// * arterial link *
		if(	(iLType > 19 && iLType < 31) ) {	// arterials

				// add one to both arterial nodes
				arterialNode.arr[orig] += 1;
				arterialNode.arr[dest] += 1;

				// add one to both ramp nodes
				rampNode.arr[orig] += 1;
				rampNode.arr[dest] += 1;


				// increase by 100 to specify that the arterial nodes did, in fact
				// touch an arterial link. Now we can now use if(art[n] >= 103) logic
				if(arterialNode.arr[orig] < 100)
					arterialNode.arr[orig] += 100;

				if(arterialNode.arr[dest] < 100)
					arterialNode.arr[dest] += 100;

		// * ramp link *
		} else if(  (iLType > 69 && iLType < 80) ) {		// ramp
			
				// add one to both arterial nodes
				arterialNode.arr[orig] += 1;
				arterialNode.arr[dest] += 1;

				// add one to both ramp nodes
				rampNode.arr[orig] += 1;
				rampNode.arr[dest] += 1;
		
				// increase by 100 to specify that the ramp nodes did, in fact
				// touch a ramp link. Now we can now use if(rmp[n] >= 102) logic
				if(rampNode.arr[orig] < 100)
					rampNode.arr[orig] += 100;

				if(rampNode.arr[dest] < 100)
					rampNode.arr[dest] += 100;


		// * highway or toll link *
		} else if(	(iLType > 9  && iLType < 20) ||		// highways & tolls
					(iLType > 90 && iLType < 97) ) {	// highways & tolls			


				// add one to both arterial nodes
				arterialNode.arr[orig] += 1;
				arterialNode.arr[dest] += 1;


		// * all other links *
		} else {

				rampNode.arr[orig] += 1;
				rampNode.arr[dest] += 1;
		
		}

	}

	fclose(flnx);

	return 1;
}

// ********************************************************
// *
// * Name:		getNodeCoords
// *
// * Purpose:	get the node coordinates for the nodes we
// *			care about (ramp and/or arterial)
// *
// ********************************************************
int cLanduse::getNodeCoords(cMatrix &nodes, cArrayLarge<int> &rampNode, cArrayLarge<int> &arterialNode)
{
	int		nNodes = 0;
	long	row = 0;
	long	maxNode = 0;
	long	node;
	float	x, y;
	char	buf[256];

	FILE *fnde;


	// count the number of actual nodes we will process
	for(long i = 1; i < rampNode.nRows; i++) {
		
		// don't forget to include the TAZs
		if(rampNode.arr[i] >= 102 || arterialNode.arr[i] >= 103 || i <= LastInternalZone)
		//if(rampNode.arr[i] >= 100 || arterialNode.arr[i] >= 100 || i <= LastInternalZone)
			nNodes += 1;

	}
	
	// [][0] = node number
	// [][1] = ramp=1, arterial=2, both=3
	// [][2] = x
	// [][3] = y
	nodes.AllocateMatrix(nNodes, 4);
	

	// open the file and do data validation
	if( (fnde = fOpenFile("TXT", fNameNode, "r")) == NULL)
		return 0;


	// read header
	fgets(buf, sizeof(buf), fnde);


	// we are not going to use nNodes as an ID
	nNodes = 0;


	// loop through the entire file
	while (NULL != fgets(buf, sizeof(buf), fnde)) {
		row++;

		if( 3 != sscanf(buf, " %ld, %f, %f ", &node, &x, &y)) {
			sprintf(buf, "Failed reading node file in line %ld.", row);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			fclose(fnde);
			return 0;
		}

		if(rampNode.arr[node] >= 102 || arterialNode.arr[node] >= 103 || node <= LastInternalZone) {
		//if(rampNode.arr[node] >= 100 || arterialNode.arr[node] >= 100 || node <= LastInternalZone) {

			nodes.mat[nNodes][0] = node;
			//nodes.mat[nNodes][1] = (rampNode.arr[node] > 102) + ((arterialNode.arr[row] > 103) * 2);
			//nodes.mat[nNodes][1] = (rampNode.arr[node] > 100) + ((arterialNode.arr[row] > 100) * 2);
			nodes.mat[nNodes][1] = (rampNode.arr[node] > 102) + ((arterialNode.arr[node] > 103) * 2);
			nodes.mat[nNodes][2] = x;
			nodes.mat[nNodes][3] = y;

			nNodes += 1;		
		}
	}

	fclose(fnde);


	if(this->cfgDebug) {	

		// make a file name for the TAZ output file
		if( !MakeFileName(fOutName, ".AND", buf) )
			return 0;

		// open an output file for dumping the closest TAZs
		if( (fnde = fOpenFile("TXT", buf, "w")) == NULL)
			return 0;

		fprintf(fnde, "Node, Type, X, Y, ramp, arterial\n");

		for(int i = 0; i < nNodes; i++) 
			fprintf(fnde, "%ld, %ld, %ld, %ld, %ld, %ld\n", (long)nodes.mat[i][0], (long)nodes.mat[i][1], (long)nodes.mat[i][2], (long)nodes.mat[i][3], (long)rampNode.arr[(long)nodes.mat[i][0]], (long)arterialNode.arr[(long)nodes.mat[i][0]]); 

		fclose(fnde);
		
	}

	return 1;
}

// ********************************************************
// *
// * Name:		calculateDistances
// *
// * Purpose:	now calculate the distances from each taz
// *			to the known intersections)
// *
// ********************************************************
int cLanduse::calculateDistances(cTAZ &oTAZ, cMatrix &nodes)
{
	int		i, j;
	int		nNodes = nodes.nRows;
	float	x, y;
	char	buf[256];
	FILE	*fnde;

	if(this->cfgDebug) {	

		// make a file name for the TAZ output file
		if( !MakeFileName(fOutName, ".CND", buf) )
			return 0;

		// open an output file for dumping the closest TAZs
		if( (fnde = fOpenFile("TXT", buf, "w")) == NULL)
			return 0;

		fprintf(fnde, "node, closestRmp, RmpDist, closestArt, ArtDist\n");
	}


	// loop through all zones
	for(i = 0; i < LastInternalZone; i++) {
		int		jRmp = 0;
		int		jArt = 0;
		float	dArt = 99999;
		float	dRmp = 99999;

		x = nodes.mat[i][2];
		y = nodes.mat[i][3];

		// loop through all other nodes
		for(j = LastInternalZone; j < nNodes; j++) {
			int typ = (int)nodes.mat[j][1];

			float d = sqrt ( ((x - nodes.mat[j][2]) * (x - nodes.mat[j][2])) +
			 				 ((y - nodes.mat[j][3]) * (y - nodes.mat[j][3])) );
		

			switch (typ) {
				case 1:				
					if(d < dRmp) {
						dRmp = d;
						jRmp = j;
					}

					break;

				case 2:
					if(d < dArt) {
						dArt = d;
						jArt = j;
					}

					break;

				case 3:
					if(d < dRmp) {
						dRmp = d;
						jRmp = j;
					}

					if(d < dArt) {
						dArt = d;
						jArt = j;
					}

					break;
			}
		}


		// note: this is the sqrt of the distance
		//oTAZ.Recs[i].distToRamp = sqrt( sqrt(dRmp / 1604.8) ) + 0.1;
		oTAZ.Recs[i].distToRamp = sqrt( sqrt(dRmp / 1609.344) ) + 0.1;
		oTAZ.Recs[i].distToArterial = sqrt( sqrt(dArt / 1609.344) ) + 0.1;


		if(this->cfgDebug) {	
			fprintf(fnde, "%ld, %ld, %f, %ld, %f\n", (long)nodes.mat[i][0], (long)nodes.mat[jRmp][0], dRmp, (long)nodes.mat[jArt][0], dArt); 
		}
	
		// only do page reporting after every n zones
		if( !((i+1) % 100)){
 			sprintf(buf, "Int distance for zone %d", i + 1);
			header(buf);
		}

	}

	if(this->cfgDebug) {	
		fclose(fnde);
	}

	return 1;
}

// ********************************************************
// *
// * Name:		CalculateLandConsumption
// *
// * Purpose:	
// *
// ********************************************************
int cLanduse::CalculateLandConsumption(cTAZ &oTAZ)
{
	int i;


	for(i = 0; i < LastInternalZone; i++) {
		float accessChange = 1.0;
		float resDensity = 0;
		float nonresPercentDeveloped = 0;
		float percentDeveloped = 1.0;

		// number of developed acres
		float devAcres = oTAZ.Recs[i].resDeveloped + oTAZ.Recs[i].nonresDeveloped;

		// calculate # of acres available for development
		float  availableAcres = max(1, oTAZ.Recs[i].totalAcres - (devAcres + oTAZ.Recs[i].undevelopableAcres) );
		
		// percent developed
		if( (devAcres + availableAcres) > 0 && availableAcres > 1)
			percentDeveloped = devAcres / ( devAcres + availableAcres);

		// percent of area developed by non-residential
		if(devAcres > 0)
			nonresPercentDeveloped = oTAZ.Recs[i].nonresDeveloped / devAcres;

		if( oTAZ.Recs[i].cur_AdjTTimeWgtByHH_Emp > 0)
			accessChange = min(1, ((oTAZ.Recs[i].avgAdjTTimeWgtByHH_Emp - oTAZ.Recs[i].cur_AdjTTimeWgtByHH_Emp) / oTAZ.Recs[i].cur_AdjTTimeWgtByHH_Emp) );
			
		// the log of housing density
		if( oTAZ.Recs[i].resDeveloped > 0)
			resDensity = max(0, log( (oTAZ.Recs[i].housing / oTAZ.Recs[i].resDeveloped) + 0.01) );

		// LLACHE = Log of the ACcess weighted by Housing and Employment
		float llache = log(oTAZ.Recs[i].avgAdjTTimeWgtByHH_Emp + 3);

		// *********************************************

		// residential land consumption
		float resCons = resConstantCoef +												// constant
						resGrowBoundaryCoef * (oTAZ.Recs[i].growthBoundary == 0) +		// UGB = urban growth boundary
						resDistToCoast * min(10, oTAZ.Recs[i].distToCoast) +			// dist_coast = distance to coast
						resAccessChangeCoef * accessChange +							// pache_2 = change in travel time
						resNonResUseCoef * nonresPercentDeveloped +						// nr_cnt_per = non residental percent developed
						resCoastalTAZCoef * (oTAZ.Recs[i].distToCoast <= 5) +			// coastal = is the coast within 5 miles
						resDevelopmentCoef * percentDeveloped +							// dev_pct = percent already developed
						resNonResCoastalCoef * ((oTAZ.Recs[i].distToCoast <= 5) * nonresPercentDeveloped) +	// _InrXco = non residential percent development within 5 miels of the coast
						resAccessCoef[ oTAZ.Recs[i].accessCategorical ] +				// _llache_cat_n = categorical access
						resSmallVacantCoef * ( oTAZ.Recs[i].resAvailableAcres < 1) +	// _lland_1 = land area less than 1 acre
						resLargeVacantCoef * ( oTAZ.Recs[i].resAvailableAcres > 1000);	// _lland_2 = land area greater than 1000 acres

		resCons = exp(resCons);
		oTAZ.Recs[i].landConsuptionHH = resCons / (1 + resCons);


		// non residential land consumption
		float nonresCon =	nonresConstantCoef +
							nonresGrowBoundaryCoef * (oTAZ.Recs[i].growthBoundary == 0) +		// UGB = urban growth boundary			
							nonresDistToRampCoef * oTAZ.Recs[i].distToRamp + 
							nonresAccessChangeCoef * accessChange +
							nonresAccessCoeff[ oTAZ.Recs[i].accessCategorical ];
			

		nonresCon = exp(nonresCon);
		oTAZ.Recs[i].landConsuptionEmp = nonresCon / (1 + nonresCon);


		// *********************************************

		// residential density
		float resDens = d_resConstantCoef + 
						d_resDevelopmentDensCoef * resDensity + 
						d_resAccessChangeCoef * accessChange + 
						d_resSmallVacantCoef * ( oTAZ.Recs[i].resAvailableAcres < 1) +
						d_resLargeVacantCoef * ( oTAZ.Recs[i].resAvailableAcres > 1000) +
						d_resAccessCoef[ oTAZ.Recs[i].accessCategorical ];


		oTAZ.Recs[i].landDensityHH = resDens;



		// non residential density
		float nonResDens =	d_nonresConstantCoef + 
							d_nonresAccessCoef * llache;


		oTAZ.Recs[i].landDensityEmp = nonResDens;
	}


	return 1;
}

// ********************************************************
// *
// * Name:		CalculateNewHousing
// *
// * Purpose:	
// *
// ********************************************************
int cLanduse::CalculateNewHousing(cTAZ &oTAZ, cMatrix &oGrowthIncrement)
{
	int		i;
	char	buf[256];

	int		fConverged = 1;

	cArray<float> HH_Check_GrowthCenter(oTAZ.nGrowthCenter + 1);		

	cArray<float> DRIHHbyGrowthCenter(oTAZ.nGrowthCenter + 1);		
	cArray<float> HHbyGrowthCenter(oTAZ.nGrowthCenter + 1);		
	cArray<float> ScaleFactorForGrowthCenter(oTAZ.nGrowthCenter + 1);		

	cArray<float> rsgRd2(LastInternalZone);		
	cArray<float> resVac3(LastInternalZone);		

	// **********************
	// STEP 1: Calculate RSG HHs.
	// this is an unfactored calculation of the households
	// **********************
	for(i = 0; i < LastInternalZone; i++) {
		float	resDensity = 0;
		float	expConsumption;
		float	expDensity;


		// CALCULATE factor the consumption
		if( oTAZ.Recs[i].resDeveloped > 0)
			resDensity = oTAZ.Recs[i].housing / oTAZ.Recs[i].resDeveloped;


		// CALCULATE residential vacant land : resvac3 (row AR)
		resVac3.arr[i] = oTAZ.Recs[i].resAvailableAcres;


		// CALCULATE expDensity (row AO)
		expDensity = exp( oTAZ.Recs[i].landDensityHH );


		// CALCULATE rsg_rd2 (row AP)
		if(expDensity > 0) {

			if( (resDensity / expDensity) < 20) {

				if( oTAZ.Recs[i].totalAcres < 1000 )
					rsgRd2.arr[i] = max( expDensity, resDensity);
				else 
					rsgRd2.arr[i] = expDensity;
								
			} else {
				rsgRd2.arr[i] = expDensity;
			}
		}

		// apply density constraint
		if (oTAZ.Recs[i].housingDensityConstraint != 1.0 && rsgRd2.arr[i] > oTAZ.Recs[i].housingDensityConstraint)
			rsgRd2.arr[i] = oTAZ.Recs[i].housingDensityConstraint;

		// CALCULATE temporary household allocation
		oTAZ.Recs[i].HHAllocated = resVac3.arr[i] * rsgRd2.arr[i] * oTAZ.Recs[i].landConsuptionHH;


	}


	// **********************
	// STEP 2: now add the totals up by growth center
	// this will be used to factor the above numbers
	// **********************

	// sum the initial housing by growth center (probably county)
	for(i = 0; i < LastInternalZone; i++) {
		int gCenter = oTAZ.Recs[i].growthCenter;

		if(gCenter < 1 || gCenter > oTAZ.nGrowthCenter) {
			sprintf(buf, "Found %d growth region(s) in oTAZ file where max is set at %d (CalculateNewHousing)", gCenter, oTAZ.nGrowthCenter);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			return 0;
		}

		DRIHHbyGrowthCenter.arr[ gCenter ] += oTAZ.Recs[i].DRI_Housing;

		HHbyGrowthCenter.arr[ gCenter ] += oTAZ.Recs[i].HHAllocated;
	}


	// now calculate the growth factor to be used to scale growth to growth center
	for(i = 1; i <= oTAZ.nGrowthCenter; i++) {
		
		// housing control total / initial estimate of housing
		if(HHbyGrowthCenter.arr[i] > 0) 
			ScaleFactorForGrowthCenter.arr[i] = (oGrowthIncrement.mat[i][0] - DRIHHbyGrowthCenter.arr[i]) / HHbyGrowthCenter.arr[i];	

	}


	// **********************
	// STEP 3: now calulate the final households using
	// the factors calculated above
	// **********************

	for(i = 0; i < LastInternalZone; i++) {

		// CALCULATION get the growth center
		int gCenter = oTAZ.Recs[i].growthCenter;


		// CALCULATION factor the original consumption 
		oTAZ.Recs[i].resFactoredCons = min(1, oTAZ.Recs[i].landConsuptionHH * ScaleFactorForGrowthCenter.arr[gCenter]);


		// to support iteration
		oTAZ.Recs[i].landConsuptionHH = oTAZ.Recs[i].resFactoredCons;

		// we don't have resFactoredDensity so use our calculated one
		oTAZ.Recs[i].resFactoredDensity = rsgRd2.arr[i];
		
		// CALCULATE final household allocation
		oTAZ.Recs[i].HHAllocated  = resVac3.arr[i] * oTAZ.Recs[i].resFactoredCons * oTAZ.Recs[i].resFactoredDensity;


		// sum housing up by county
		HH_Check_GrowthCenter.arr[gCenter] += oTAZ.Recs[i].HHAllocated;


		// CALCULATE new household total
		// assumng this is the final step, add the DRIs to the allocated. If it is not the final step
		// then the allocated will be recalculated
		oTAZ.Recs[i].HHTotal  = oTAZ.Recs[i].housing + oTAZ.Recs[i].HHAllocated + oTAZ.Recs[i].DRI_Housing;
	}


	// compare results to control totals
	for(i = 1; i <= oTAZ.nGrowthCenter; i++) {

		if( fabs((HH_Check_GrowthCenter.arr[i] + oTAZ.Recs[i].DRI_Housing) - HHbyGrowthCenter.arr[i]) > 100)
			fConverged = -100;		
	}


	return fConverged;
}


// ********************************************************
// *
// * Name:		CalculateNewEmployment
// *
// * Purpose:	
// *
// ********************************************************
int cLanduse::CalculateNewEmployment(cTAZ &oTAZ, cMatrix &oGrowthIncrement)
{
	int		i;
	char	buf[256];

	int		fConverged = 1;

	cArray<float> Emp_Check_GrowthCenter(oTAZ.nGrowthCenter + 1);		

	cArray<float> DRIEmpbyGrowthCenter(oTAZ.nGrowthCenter + 1);		
	cArray<float> EmpbyGrowthCenter(oTAZ.nGrowthCenter + 1);		
	cArray<float> ScaleFactorForGrowthCenter(oTAZ.nGrowthCenter + 1);		

	cArray<float> density(LastInternalZone);		
	cArray<float> empVacLand(LastInternalZone);		


	// **********************
	// STEP 1: Calculate RSG employment.
	// this is an unfactored calculation of the employment
	// **********************
	for(i = 0; i < LastInternalZone; i++) {
		float	nonresDensity = 0;

		// CALCULATE non residential density (emp / nonresdev)
		if( oTAZ.Recs[i].nonresDeveloped + 1 != 0)
			nonresDensity = oTAZ.Recs[i].employment / (oTAZ.Recs[i].nonresDeveloped + 1);


		// CALCULATE non residential vacant land : empVacLand
		empVacLand.arr[i] = oTAZ.Recs[i].nonresAvailableAcres;


		// CALCULATE get the calculated density model value
		float rsg_rd = max(0, oTAZ.Recs[i].landDensityEmp);


		// CALCULATE revised density
		if(oTAZ.Recs[i].totalAcres > 1000) {

			if(nonresDensity > 50) 
				density.arr[i] = max(5, rsg_rd);
			else
				density.arr[i] = max(5, nonresDensity);
		
		} else {
			density.arr[i] = max(5, max(nonresDensity, rsg_rd));
		}


		// apply density constraint
		if (oTAZ.Recs[i].employmentDensityConstraint != 1000.0 && density.arr[i] > oTAZ.Recs[i].employmentDensityConstraint)
			density.arr[i] = oTAZ.Recs[i].employmentDensityConstraint;
		
		oTAZ.Recs[i].EmpAllocated = oTAZ.Recs[i].landConsuptionEmp * density.arr[i] * empVacLand.arr[i];

	
	}
		
	// **********************
	// STEP 2: now add the totals up by growth center
	// this will be used to factor the above numbers
	// **********************

	// sum the initial employment by growth center (probably county)
	for(i = 0; i < LastInternalZone; i++) {
		int gCenter = oTAZ.Recs[i].growthCenter;

		if(gCenter < 1 || gCenter > oTAZ.nGrowthCenter) {
			sprintf(buf, "Found %d growth region(s) in oTAZ file where max is set at %d (CalculateNewHousing)", gCenter, oTAZ.nGrowthCenter);
			MessageBox (hMainWnd, buf, "Error",	MB_ICONSTOP);
			return 0;
		}

		DRIEmpbyGrowthCenter.arr[ gCenter ] += oTAZ.Recs[i].DRI_Employment;

		EmpbyGrowthCenter.arr[ gCenter ] += oTAZ.Recs[i].EmpAllocated;
	}


	// now calculate the growth factor to be used to scale growth to growth center
	for(i = 1; i <= oTAZ.nGrowthCenter; i++) {
		
		// employment control total / initial estimate of employment
		// use max(0, control total) because we have some negatives in the control totals
		if(EmpbyGrowthCenter.arr[i] > 0) 
			ScaleFactorForGrowthCenter.arr[i] = max(0, (oGrowthIncrement.mat[i][1] - DRIEmpbyGrowthCenter.arr[i]) / EmpbyGrowthCenter.arr[i]);	

	}


	// **********************
	// STEP 3: now calulate the final employment using
	// the factors calculated above
	// **********************

	for(i = 0; i < LastInternalZone; i++) {


		// CALCULATION get the growth center
		int gCenter = oTAZ.Recs[i].growthCenter;


		// CALCULATION factor the original consumption 
		oTAZ.Recs[i].nonresFactoredCons = min(1, oTAZ.Recs[i].landConsuptionEmp * ScaleFactorForGrowthCenter.arr[gCenter]);


		// to support iteration
		oTAZ.Recs[i].landConsuptionEmp = oTAZ.Recs[i].nonresFactoredCons;


		// we don't have nonresFactoredDensity so use our calculated one
		oTAZ.Recs[i].nonresFactoredDensity = density.arr[i];

		// CALCULATE final employment allocation
		oTAZ.Recs[i].EmpAllocated  = empVacLand.arr[i] * oTAZ.Recs[i].nonresFactoredCons * oTAZ.Recs[i].nonresFactoredDensity;


		// sum housing up by county
		Emp_Check_GrowthCenter.arr[gCenter] += oTAZ.Recs[i].EmpAllocated;


		// CALCULATE new employment total
		// assumng this is the final step, add the DRIs to the allocated. If it is not the final step
		// then the allocated will be recalculated
		oTAZ.Recs[i].EmpTotal  = oTAZ.Recs[i].employment + oTAZ.Recs[i].EmpAllocated + oTAZ.Recs[i].DRI_Employment;

	}


	// compare results to control totals
	for(i = 1; i <= oTAZ.nGrowthCenter; i++) {

		if( fabs((Emp_Check_GrowthCenter.arr[i] + oTAZ.Recs[i].DRI_Employment) - EmpbyGrowthCenter.arr[i]) > 100)
			fConverged = -100;		
	}


	return fConverged;
}


// ********************************************************
// *
// * Name:		CalculateFratarInputFile
// *
// * Purpose:	
// *
// ********************************************************
int cLanduse::CalculateFratarOutput(cTAZ &oTAZ)
{
	int i;
	float growth;
	char buf[256];

	FILE *f;
	
	if( (f = fOpenFile("TXT", fOutNameFratar, "w")) == NULL)
		return 0;

	for(i = 0; i < LastInternalZone; i++) {
		
		// areatype values
		// R = 1
		// U = 2
		// T = 3


		// make this a zero based value
		int		at = oTAZ.Recs[i].areaType - 1;

		float baseTrips =	fratConstant + 
							oTAZ.Recs[i].fratEmployment * fratEmpFact[at] +
							oTAZ.Recs[i].fratHousing * fratHHFact + 
							fratUrbanArea * (oTAZ.Recs[i].areaType == 2);

		float futrTrips =	fratConstant + 
							oTAZ.Recs[i].EmpTotal * fratEmpFact[at] +
							oTAZ.Recs[i].HHTotal * fratHHFact + 
							fratUrbanArea * (oTAZ.Recs[i].areaType == 2);


		if( baseTrips > 0 ) {
			growth = futrTrips / baseTrips;
		} else {
			growth = 1;
		}


		if(oTAZ.Recs[i].taz < 10)
			sprintf(buf, "FO 000%d", oTAZ.Recs[i].taz);
		else if(oTAZ.Recs[i].taz < 100)
			sprintf(buf, "FO 00%d", oTAZ.Recs[i].taz);
		else if(oTAZ.Recs[i].taz < 1000)
			sprintf(buf, "FO 0%d", oTAZ.Recs[i].taz);
		else
			sprintf(buf, "FO %d", oTAZ.Recs[i].taz);
	
		
		long iGrowth = long((growth + .005) * 100);

		
		if(iGrowth < 100)
			sprintf(buf, "%s 1 00000%d", buf, iGrowth);
		else if(iGrowth < 1000)
			sprintf(buf, "%s 1 0000%d", buf, iGrowth);
		else if(iGrowth < 10000)
			sprintf(buf, "%s 1 000%d", buf, iGrowth);
		else if(iGrowth < 100000)
			sprintf(buf, "%s 1 00%d", buf, iGrowth);
		else if(iGrowth < 1000000)
			sprintf(buf, "%s 1 0%d", buf, iGrowth);
		else
			sprintf(buf, "%s 1 %d", buf, iGrowth);

		fprintf(f, "%s\n", buf);
	}

	fclose(f);

	return 1;
}
