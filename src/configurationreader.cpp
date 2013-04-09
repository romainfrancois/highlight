/***************************************************************************
                          configurationreader.cpp  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/


/*
This file is part of Highlight.

Highlight is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Highlight is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Highlight.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "configurationreader.h"

#include <string>
#include <sstream>
#include <map>
#include <iostream>
#include <fstream>
#include <vector>

#include "stringtools.h"

using namespace std;

ConfigurationReader::ConfigurationReader ( const string & configuration_path )
{
	ifstream in ( configuration_path.c_str() );
	fileFound=in;
	if ( fileFound )
	{
		string line;
		line.reserve ( 500 );
		size_t lineBegin;
		size_t delimPos;
		string paramName;
		char suffix[10];
		int i=0;

		while ( getline ( in, line ) )
		{
			lineBegin=line.find_first_not_of ( "\t " );
			if ( ( line.size() >2 ) && ( lineBegin!=string::npos )
			        && ( line.at ( lineBegin ) !='#' ) )    //comment?
			{
				if ( line[lineBegin]=='$' )       // new parameter?
				{
					delimPos=line.find ( "=",lineBegin )-1;
					if ( delimPos!=string::npos )
					{
						paramName=StringTools::trimRight (
						              StringTools::change_case ( line.substr ( lineBegin+1, delimPos ) ) );
						// if parameter already exists, make it unique
						if ( parameterMap.count ( paramName ) )
						{
							// snprintf ( suffix, sizeof ( suffix ), "#%05d", ++i );
							sprintf ( suffix, "#%05d", ++i );
							paramName+=suffix;
						}
						parameterNames.push_back ( paramName );
						parameterMap[paramName] = line.substr ( delimPos+2, line.length() );
					}
				}
				else
				{
					parameterMap[paramName]+= ( " "+line );
				}
			}
		}
		in.close();
	}
}

ConfigurationReader::~ConfigurationReader()
{
}

bool ConfigurationReader::found()
{
	return fileFound;
}

string &ConfigurationReader::getParameter ( const string & paramName )
{
	return parameterMap[paramName] ;
}

const char* ConfigurationReader::getCParameter ( const string & paramName )
{
	return parameterMap[paramName].c_str() ;
}

vector<string> &ConfigurationReader::getParameterNames()
{
	return parameterNames;
}


