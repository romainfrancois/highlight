/***************************************************************************
                          languagedefinition.cpp  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001-2007 by Andre Simon
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


#include <memory>

#include "languagedefinition.h"
#include "stringtools.h"


using namespace std;

namespace highlight
{

	const string LanguageDefinition::REGEX_IDENTIFIER =
	    "[a-zA-Z_]\\w*";

	const string LanguageDefinition::REGEX_NUMBER =
	    "(?:0x|0X)[0-9a-fA-F]+|\\d*[\\.]?\\d+(?:[eE][\\-\\+]\\d+)?[lLuU]*";

	LanguageDefinition::LanguageDefinition() :
			ignoreCase ( false ),
			disableHighlighting ( false ),
			allowExtEscape ( false ),
			allowNestedComments ( true ),
			reformatCode ( false )
	{
		reDefPattern=Pattern::compile ( "^regex\\((.+?)(,\\s*(\\d+))?\\)$" );
	}

	LanguageDefinition::~LanguageDefinition()
	{
		for ( vector<RegexElement*>::iterator it=regex.begin(); it!=regex.end();it++ )
		{
			delete *it;
		}
		delete reDefPattern;
	}

	int LanguageDefinition::isKeyword ( const string &s )
	{
		return ( s.length() && keywords.count ( s ) ) ? keywords[s] : 0;
	}

	void LanguageDefinition::addSymbol ( stringstream& symbolStream,
	                                     State stateBegin,
	                                     State stateEnd,
	                                     bool isDelimiter,
	                                     const string& paramValue,
	                                     unsigned int classID )
	{
		RegexDef re = extractRegex ( paramValue );
		if ( !re.reString.empty() )
		{
			Pattern* p = Pattern::compile ( re.reString );
			if ( p!=NULL ) {
				regex.push_back ( new RegexElement ( stateBegin,stateEnd, p, re.capturingGroup ) );
			} else {
				failedRegex = re.reString;
			}
			return;
		}
		if ( isDelimiter )
		{
			addDelimiterSymbol ( symbolStream, stateBegin, stateEnd, paramValue, classID );
		}
		else
		{
			addSimpleSymbol ( symbolStream, stateBegin,paramValue );
		}
	}

	RegexDef LanguageDefinition::extractRegex ( const string &paramValue )
	{
		RegexDef re_def;
		unique_ptr<Matcher> m ( reDefPattern->createMatcher ( paramValue ) );
		if ( m.get() && m->matches() )
		{
			re_def.reString = m->getGroup ( 1 );
			if ( m->getStartingIndex ( 3 ) !=-1 )
			{
				StringTools::str2num<int> ( re_def.capturingGroup, m->getGroup ( 3 ), std::dec );
				//std::cerr << "capturingGroup "<<re_def.capturingGroup<<"\n";
			}
		}
		return re_def;
	}

	void LanguageDefinition::addSimpleSymbol ( stringstream& symbolStream,
	        State state,
	        const string& paramValue )
	{
		istringstream valueStream ( paramValue );
		bool valExists=false;
		string value;
		int pairCount =0;
		while ( valueStream >> value )
		{
			symbolStream << " " << value;
			valExists = true;
			delimiterPair[value] = ++pairCount;
		}
		if ( valExists )
		{
			symbolStream << " " << state;
		}
	}

	void LanguageDefinition::addDelimiterSymbol ( stringstream& symbolStream,
	        State stateBegin, State stateEnd,
	        const string& paramValue,
	        unsigned int classID )
	{
		istringstream valueStream ( paramValue );
		string delimPrefix, delimSuffix;
		int pairCount =0;
		while ( valueStream>>delimPrefix )
		{
			valueStream >> delimSuffix;
			symbolStream << " "<<delimPrefix <<" " << stateBegin;
			symbolStream <<" "<< delimSuffix<<" "<< stateEnd;
			delimiterPrefixes.insert ( make_pair ( delimPrefix, classID ) );
			// if no closing delimiter exists, open and close delims are equal:
			delimiterDistinct[stateBegin] = !delimSuffix.empty();
			++pairCount;
			delimiterPair[delimPrefix] = delimiterPair[delimSuffix] = pairCount;
			//std::cout << "pair: "<< delimPrefix<<"->"<<delimiterPair[delimPrefix]
			//          <<", "<<delimSuffix<<"->"<<delimiterPair[delimSuffix]<<"\n";
		}
	}

	void LanguageDefinition::addDelimiterRegex ( stringstream& symbolStream,
	        State stateBegin, State stateEnd,
	        const string& paramValue, const string& langName  )
	{
		istringstream valueStream ( paramValue );
		string delimStart, delimEnd;
		valueStream>>delimStart;
		valueStream>>delimEnd;

		RegexDef reStart = extractRegex ( delimStart );
		if ( !reStart.reString.empty() )
		{
			Pattern* p = Pattern::compile ( reStart.reString );
			if ( p!=NULL ) {
				regex.insert (regex.begin(),1,  new RegexElement ( stateBegin,stateBegin, p, reStart.capturingGroup, -1, langName ) );
			}
		}
		// end regex string needs to be saved to pass it back to host language when embedded section is over
		//host language definiton needs to know end delimiter to recognize single line embedded sections
		RegexDef reEnd = extractRegex ( delimEnd ); 
		if ( !reEnd.reString.empty() )
		{
			exitDelimiters[getNewPath(langName)]=reEnd.reString;
		}
	}

	void LanguageDefinition::restoreLangEndDelim(const string& langPath){
		if ( !langPath.empty()&& exitDelimiters.count(langPath) )
		{
			Pattern* p = Pattern::compile ( exitDelimiters[langPath]);
			if ( p!=NULL ) {
				regex.insert (regex.begin(),1, new RegexElement ( EMBEDDED_CODE_END,EMBEDDED_CODE_END, p ) );
			}
			//else
			//	cerr<<"Pattern::compile  fehler\n";
		}
	}


	void LanguageDefinition::getFlag ( string& paramValue, bool &flag)
	{
		if (paramValue.size()) flag= StringTools::change_case ( paramValue ) =="true";
	}

	void LanguageDefinition::getSymbol ( const string& paramValue, unsigned char &symbol )
	{
		if (paramValue.empty()) return;
		symbol=paramValue[0];

/*
		istringstream valueStream ( paramValue );
		unsigned char symbol;
		valueStream >> symbol;
		return symbol;
*/
	}

	void LanguageDefinition::addKeywords ( const string &kwList,
	                                       State stateBegin, State stateEnd,
	                                       int classID )
	{
		RegexDef re = extractRegex ( kwList );
		if ( !re.reString.empty() )
		{
			Pattern* p = Pattern::compile ( re.reString );
			if ( p!=NULL )
				regex.push_back ( new RegexElement ( stateBegin,stateEnd, p, classID, re.capturingGroup ) );
			else
				failedRegex = re.reString;
			return;
		}
		istringstream valueStream ( kwList );
		string keyword;
		while ( valueStream >> keyword )
		{
			keywords.insert ( make_pair ( keyword, classID ) );
		}
	}

	unsigned int LanguageDefinition::generateNewKWClass ( const string& newClassName )
	{
		unsigned int newClassID=0;
		bool found=false;
		while ( newClassID<keywordClasses.size() && !found )
		{
			found = ( newClassName==keywordClasses[newClassID++] );
		}
		if ( !found )
		{
			newClassID++;
			keywordClasses.push_back ( newClassName );
		}
		return newClassID;
	}

	bool LanguageDefinition::load ( const string& langDefPath, bool clear )
	{
		if ( clear )  reset();

		ConfigurationReader langDef ( langDefPath );
		if ( !langDef.found() )
		{
			currentPath.clear();
			return false;
		}
		currentPath=langDefPath;
		disableHighlighting=false;
		string token;
		stringstream symbolStrStream;

		addSymbol ( symbolStrStream,
		            STRING,
		            STRING_END,
		            false,
		            langDef.getParameter ( "stringdelimiters" ) );

		addSymbol ( symbolStrStream,
		            STRING,
		            STRING_END,
		            true,
		            langDef.getParameter ( "string_unequal" ) );

		addSymbol ( symbolStrStream,
		            DIRECTIVE,
		            DIRECTIVE_END,
		            false,
		            langDef.getParameter ( "directive" ) );

		addSymbol ( symbolStrStream,
		            ESC_CHAR,
		            ESC_CHAR_END,
		            false,
		            langDef.getParameter ( "escchar" ) );

		addSymbol ( symbolStrStream,
		            SL_COMMENT,
		            SL_COMMENT_END,
		            false,
		            langDef.getParameter ( "sl_comment" ) );

		addSymbol ( symbolStrStream,
		            ML_COMMENT,
		            ML_COMMENT_END,
		            true,
		            langDef.getParameter ( "ml_comment" ) );

		addSymbol ( symbolStrStream,
		            ML_COMMENT,
		            ML_COMMENT_END,
		            false,
		            langDef.getParameter ( "ml_comment_equal" ) );

		addSymbol ( symbolStrStream,
		            SYMBOL,
		            SYMBOL_END,
		            false,
		            langDef.getParameter ( "symbols" ) );

		string paramName, className, classValue;
		vector<string> paramNames=langDef.getParameterNames();
		for ( unsigned int i=0;i<paramNames.size();i++ )
		{
			paramName=paramNames[i];
			className=StringTools::getParantheseVal ( paramName );
			classValue=langDef.getParameter ( paramName );
			if ( paramName.find ( "keywords" ) != string::npos )
			{
				addKeywords ( classValue, KEYWORD, KEYWORD_END, generateNewKWClass ( className ) );
			} else if (paramName.find ( "nested" ) != string::npos) {
				addDelimiterRegex( symbolStrStream, EMBEDDED_CODE_BEGIN, EMBEDDED_CODE_END,
		            			classValue, className );
			}
		}

		// use hardcoded regex if not defined in language definition
		// TODO save as members to alloe redefinition in langdefs with include stmt
		string user_def_re = extractRegex ( langDef.getParameter ( "digit" ) ).reString;
		string re_digit = ( user_def_re.empty() ) ? REGEX_NUMBER : user_def_re;

		user_def_re = extractRegex ( langDef.getParameter ( "identifier" ) ).reString;
		string re_identifier= ( user_def_re.empty() ) ? REGEX_IDENTIFIER: user_def_re;

		// insert identifier and number regex after keyword regexes
		regex.push_back ( new RegexElement ( IDENTIFIER_BEGIN, IDENTIFIER_END,
		                                     Pattern::compile ( re_identifier ) ) );
		regex.push_back ( new RegexElement ( NUMBER, NUMBER_END,
		                                     Pattern::compile ( re_digit ) ) );

		symbolString = symbolStrStream.str();

		getFlag ( langDef.getParameter ( "ignorecase" ),  ignoreCase);
		getFlag ( langDef.getParameter ( "allownestedcomments" ), allowNestedComments );
		getFlag ( langDef.getParameter ( "disablehighlighting" ), disableHighlighting );
		getFlag ( langDef.getParameter ( "reformatting" ), reformatCode );
		getSymbol ( langDef.getParameter ( "rawstringprefix" ), rawStringPrefix );
		getSymbol ( langDef.getParameter ( "continuationsymbol" ), continuationChar );
		getFlag ( langDef.getParameter ( "allowextescape" ), allowExtEscape);

		langDesc = langDef.getParameter ( "description" );

		//load syntax before it is overridden by calling language definition
		string fileToInclude=langDef.getParameter ( "include" );
		if ( !fileToInclude.empty() )
		{
			//string::size_type Pos = langDefPath.find_last_of ( Platform::pathSeparator );
			//string includeLangDefPath = langDefPath.substr ( 0, Pos+1 ) + fileToInclude;
			//load ( includeLangDefPath, false );
			load(getNewPath(fileToInclude), false);
		}

		return failedRegex.empty();
	}

	void LanguageDefinition::reset()
	{
		keywords.clear();
		keywordClasses.clear();
		delimiterPrefixes.clear();
		delimiterDistinct.clear();
		delimiterPair.clear();
		langDesc.clear();
		ignoreCase= false;
		allowNestedComments= reformatCode = false;
		rawStringPrefix = continuationChar = '\0';
		disableHighlighting=allowExtEscape=false;

		// TODO eigene methode
		for ( vector<RegexElement*>::iterator it=regex.begin(); it!=regex.end();it++ )
		{
			delete *it;
		}
		regex.clear();
		failedRegex.clear();
	}

	string LanguageDefinition::getNewPath(const string& lang){
			string::size_type Pos = currentPath.find_last_of ( Platform::pathSeparator );
			return currentPath.substr ( 0, Pos+1 ) + lang + ".lang";
	}

}
