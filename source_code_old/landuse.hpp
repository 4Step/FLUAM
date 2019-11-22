/***************************************************************************
 *
 * Authors:		Stephen Lawe, Resource Systems Group, slawe@rsginc.com
 *
 * Purpose:		Land use allocation
 *
 * Input files:	
 *				ALU = allowable land use per zone
 *				FZR	= bus fare zone structure
 *				RZR = rail fare zone structure
 *				LUX = existing land use
 *				PAR = parameter file
 *				PER = permitted land use (exogenously added)
 *				TFR = transit frequency matrix (buses per hour)
 *				TRT = transit travel time
 *				TTY = vehicle travel time matrix
 *				ZON = zone specific information (sed, stu, tni, zcf)
 *
 * output files:
 *				LUX = new land use totals
 *				ALN = just the increment of land use allocated
 *
 * History:		See 'WinMain.cpp' for extened history
 *         		
 ***************************************************************************/

#ifndef _LANDUSE_HPP_
#define _LANDUSE_HPP_

#include "matrix.hpp"
#include "arrayLarge.hpp"
#include "zon.hpp"
#include "par.hpp"
#include "ttb.hpp"
#include "tty.hpp"
#include "lux.hpp"
#include "gro.hpp"
#include "taz.hpp"

BOOL CALLBACK LUAMdialog (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);


class cLanduse{

	// florida
	public:

		BOOL	doLUAM;

		int		cfgDebug;
		int		cfgNewSkim;
		int		cfgNewNetwork;

		int		LastInternalZone;
		int		LastExternalZone;
		int		ClosestNTAZs;
		float	TravelTimeAdjustment;

		// control file
		char	CTLName[NAMESIZE];

		// input files
		char	fNameTTY[NAMESIZE];
		char	fNameTAZ[NAMESIZE];
		char	fNameGrowth[NAMESIZE];
		char	fNameNode[NAMESIZE];
		char	fNameLink[NAMESIZE];
		char	fNameDensityConstraints[NAMESIZE];

		// output files
		char	fOutName[NAMESIZE];
		char	fOutNameNextTAZ[NAMESIZE];
		char	fOutNameFratar[NAMESIZE];

	// fratar parameters
	public:
		
		float fratConstant;
		float fratEmpFact[3];
		float fratHHFact;
		float fratUrbanArea;

	// land consumption model parameters
	public:

		// residential parameters
		float	resConstantCoef;
		float	resGrowBoundaryCoef;
		float	resDistToCoast;
		float	resAccessChangeCoef;
		float	resNonResUseCoef;
		float	resCoastalTAZCoef;
		float	resNonResCoastalCoef;
		float	resDevelopmentCoef;
		float	resSmallVacantCoef;
		float	resLargeVacantCoef;
		float	resAccessCoef[11];

		// non residential parameters
		float	nonresConstantCoef;
		float	nonresGrowBoundaryCoef;
		float	nonresDistToRampCoef;
		float	nonresAccessChangeCoef;
		float	nonresAccessCoeff[11];


	// density model parameters
	public:

		// residential parameters
		float	d_resConstantCoef;
		float	d_resDevelopmentDensCoef;
		float	d_resAccessChangeCoef;
		float	d_resSmallVacantCoef;
		float	d_resLargeVacantCoef;
		float	d_resAccessCoef[11];

		// residential parameters
		float	d_nonresConstantCoef;
		float	d_nonresAccessCoef;

	public:

		cLanduse(){

			doLUAM = false;

			cfgDebug = 0;
			cfgNewSkim = 0;
			cfgNewNetwork = 0;

			LastInternalZone = 0;
			LastExternalZone = 0;
			ClosestNTAZs = 0;
			TravelTimeAdjustment = 0;
				
			CTLName[0] ='\0';
			fNameTTY[0] ='\0';
			fNameTAZ[0] ='\0';
			fNameGrowth[0] ='\0';
			fNameNode[0] = '\0';
			fNameLink[0] = '\0';

			fOutName[0] ='\0';
			fOutNameNextTAZ[0] = '\0';
			fOutNameFratar[0] = '\0';
		}


	// florida: support for preference file
	public:

		BOOL writePrefs( ostream &out );

		BOOL readPrefs( istream &in );

	// florida: read parameters from control file
	public:

		void copyLUAM(cLanduse &s);

		BOOL readParameters(char *CTLName);

		BOOL readVer(FILE *fctl);

		BOOL readGen(FILE *fctl);

		BOOL readFrat(FILE *fctl);

		BOOL readResConsumption(FILE *fctl);

		BOOL readNonResConsumption(FILE *fctl);

		BOOL readResDensity(FILE *fctl);

		BOOL readNonResDensity(FILE *fctl);

		BOOL readFileNames(FILE *fctl);

		BOOL getInt(FILE *fctl, char *fnct, char *data, int &saveIt);

		BOOL getFloat(FILE *fctl, char *fnct, char *data, float &saveIt);

		BOOL getString(FILE *fctl, char *fnct, char *data, char *sBuf);


	// florida: LUAM functions
	public:

		int readGrowthFile(char *fNameGrowth, cMatrix &oGrowthIncrement, cTAZ &oTAZ);

		int readDensityConstraints(char *fNameDensityConstraints, cTAZ &oTAZ);

		int RunFloridaLandUse(void);

		int CalculateLandConsumption(cTAZ &oTAZ);

		int CalculateNewHousing(cTAZ &oTAZ, cMatrix &oGrowthIncrement);

		int CalculateNewEmployment(cTAZ &oTAZ, cMatrix &oGrowthIncrement);

	// methods for calculating accessability
	public:

		int calculateWeightedTTForAllTAZs(cTAZ &oTAZ);

		int CalculateWeightedTimes(cTAZ &oTAZ, cArray<float> &tempAccess, cArray<int> &tempAccessTAZ, int currOrig);

		int insertIntoArray(float *f, int *t, float time, int taz, int theMax);

		int findLocationToInsert(float *f, float time, int theMax);

		int calculateScaledAccess(cTAZ &oTAZ);

		int calculateCategoricalAccess(cTAZ &oTAZ);

	// distance to intersections
	public:

		int calculateDistToIntersections(cTAZ &oTAZ);

		int getLargestNode(void);

		int getNodeClasses(cArrayLarge<int> &rampNode, cArrayLarge<int> &arterialNode);

		int getNodeCoords(cMatrix &nodes, cArrayLarge<int> &rampNode, cArrayLarge<int> &arterialNode);

		int calculateDistances(cTAZ &oTAZ, cMatrix &nodes);

		int CalculateFratarOutput(cTAZ &oTAZ);

};


#endif

