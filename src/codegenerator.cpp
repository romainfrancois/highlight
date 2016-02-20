/***************************************************************************
                          codegenerator.cpp  -  description
                             -------------------
    begin                : Die Jul 9 2002
    copyright            : (C) 2002-2009 by Andre Simon
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


#include <climits>
#include <memory>

#include "codegenerator.h"

#include "htmlgenerator.h"
#include "xhtmlgenerator.h"
#include "rtfgenerator.h"
#include "latexgenerator.h"
#include "texgenerator.h"
#include "xmlgenerator.h"
#include "svggenerator.h"
#include "bbcodegenerator.h"
#include <Matcher.h>   
#include <astyle.h>
#include <ASStreamIterator.h>

#if !defined (QT)
#include "ansigenerator.h"
#include "xterm256generator.h"
#endif

using namespace std;

namespace highlight
{

	const unsigned int CodeGenerator::NUMBER_BUILTIN_STATES = 10;

	const string CodeGenerator::STY_NAME_STD="std";
	const string CodeGenerator::STY_NAME_STR="str";
	const string CodeGenerator::STY_NAME_NUM="num";
	const string CodeGenerator::STY_NAME_SLC="slc"; 
	const string CodeGenerator::STY_NAME_COM="com" ;
	const string CodeGenerator::STY_NAME_ESC="esc" ;
	const string CodeGenerator::STY_NAME_DIR="dir" ;
	const string CodeGenerator::STY_NAME_DST="dstr";
	const string CodeGenerator::STY_NAME_LIN="line";
	const string CodeGenerator::STY_NAME_SYM="sym" ;

	CodeGenerator * CodeGenerator::getInstance ( OutputType type )
	{
		CodeGenerator* generator=NULL;
		switch ( type )
		{
			case HTML:
				generator = new HtmlGenerator();
				break;
			case XHTML:
				generator = new XHtmlGenerator();
				break;
			case TEX:
				generator = new TexGenerator ();
				break;
			case LATEX:
				generator = new LatexGenerator();
				break;
			case RTF:
				generator = new RtfGenerator ();
				break;
			case XML:
				generator = new XmlGenerator();
				break;
			case SVG:
				generator = new SVGGenerator();
				break;
			case BBCODE:
				generator = new BBCodeGenerator();
				break;
#if !defined (QT)
			case ANSI:
				generator = new AnsiGenerator();
				break;
			case XTERM256:
				generator = new Xterm256Generator();
				break;
#endif
			default:
				break;
		}
		return generator;
	}


	CodeGenerator::CodeGenerator ( highlight::OutputType type )
			:in ( NULL ),
			out ( NULL ),
			encoding ( "none" ),
			docTitle ( "Source file" ),
			maskWs ( false ),
			excludeWs ( false ),
			fragmentOutput ( false ),
			showLineNumbers ( false ),
			lineNumberFillZeroes ( false ),
			printNewLines(true),
			lineNumber ( 0 ),
			lineNumberOffset ( 0 ),
			includeStyleDef ( false ),
			lineIndex ( 0 ),
			lineNumberWidth ( 5 ),
			maxLineCnt ( UINT_MAX ),
			terminatingChar ( '\0' ),
			formatter ( NULL ),
			formattingEnabled ( false ),
			formattingPossible ( false ),
			validateInput ( false ),
			tagsEnabled ( false ),
			noTrailingNewLine(false),
			keywordCase ( StringTools::CASE_UNCHANGED ),
			eolDelimiter ('\n'),
			outputType ( type )
	{
	}


	CodeGenerator::~CodeGenerator()
	{
		delete formatter;
	}


	bool CodeGenerator::initTheme ( const string& themePath )
	{
		this->themePath=themePath;
		bool loadOK = docStyle.load ( themePath );
		//docStyles.insert(docStyles.begin(), docStyle); // main style must be first
		initOutputTags();
		return loadOK;
	}


	bool CodeGenerator::hasWhiteBGColour()
	{
		//Colour bgCol = docStyles.front().getBgColour();
		Colour bgCol = docStyle.getBgColour();
		return bgCol.getRed ( HTML ) == "ff" && bgCol.getGreen ( HTML ) == "ff" && bgCol.getBlue ( HTML ) == "ff";
	}


	const string& CodeGenerator::getStyleName()
	{
		return themePath;
	}


	void CodeGenerator::setLineNumberWidth ( int w )
	{
		lineNumberWidth=w;
	}


	int CodeGenerator::getLineNumberWidth()
	{
		return lineNumberWidth;
	}


	void CodeGenerator::setPrintLineNumbers ( bool flag, unsigned int startCnt )
	{
		showLineNumbers=flag;
		lineNumberOffset = startCnt-1;
	}


	bool CodeGenerator::getPrintLineNumbers()
	{
		return showLineNumbers;
	}


	void CodeGenerator::setPrintZeroes ( bool flag )
	{
		lineNumberFillZeroes=flag;
	}


	bool CodeGenerator::getPrintZeroes()
	{
		return lineNumberFillZeroes;
	}


	void CodeGenerator::setIncludeStyle ( bool flag )
	{
		includeStyleDef = flag;
	}
	
	void CodeGenerator::disableTrailingNL ( bool flag )
	{
		noTrailingNewLine = flag;
	}


	void CodeGenerator::setStyleInputPath ( const string& path )
	{
		styleInputPath = path;
	}


	void CodeGenerator::setStyleOutputPath ( const string& path )
	{
		styleOutputPath = path;
	}


	const string&  CodeGenerator::getStyleInputPath()
	{
		return styleInputPath;
	}


	const string&  CodeGenerator::getStyleOutputPath()
	{
		return styleOutputPath;
	}


	void CodeGenerator::setFragmentCode ( bool flag )
	{
		fragmentOutput=flag;
	}


	bool CodeGenerator::getFragmentCode()
	{
		return fragmentOutput;
	}


	void CodeGenerator::setValidateInput ( bool flag )
	{
		validateInput=flag;
	}


	bool CodeGenerator::getValidateInput()
	{
		return validateInput;
	}


	void CodeGenerator::setBaseFont ( const string& s )
	{
		baseFont = s;
	}


	void CodeGenerator::setBaseFontSize ( const string& s )
	{
		baseFontSize = s ;
	}

	void CodeGenerator::setStartingNestedLang(const string &langName) {
		embedLangStart = langName;
	}


	const string CodeGenerator::getBaseFont() const
	{
		if ( !baseFont.empty() ) return baseFont;
		switch ( outputType )
		{
			case LATEX:
				return "ttfamily";
				break;
			case TEX:
				return "tt";
				break;
			default:
				return "Courier New";
		}
	}


	const string CodeGenerator::getBaseFontSize()
	{
		if ( baseFontSize.empty() && outputType != LATEX && outputType != TEX )
		{
			//return docStyles.front().getFontSize();
			return docStyle.getFontSize();
		}
		else
		{
			return baseFontSize;
		}
	}


	void CodeGenerator::setTitle ( const string & title )
	{
		if ( !title.empty() ) docTitle= title;
	}


	string CodeGenerator::getTitle()
	{
		return docTitle;
	}


	void CodeGenerator::setEncoding ( const string& encodingName )
	{
		encoding = encodingName;
	}


	bool CodeGenerator::formattingDisabled()
	{
		return !formattingEnabled;
	}


	void CodeGenerator::setMaxInputLineCnt ( unsigned int cnt )
	{
		maxLineCnt = cnt;
	}


	bool CodeGenerator::formattingIsPossible()
	{
		return formattingPossible;
	}


	void CodeGenerator::setPreformatting ( WrapMode lineWrappingStyle,
	                                       unsigned int lineLength,
	                                       int numberSpaces )
	{
		bool enableWrap = lineWrappingStyle!=WRAP_DISABLED;
		bool replaceTabs = numberSpaces > 0;

		if ( enableWrap || replaceTabs )
		{
			preFormatter.setWrap ( enableWrap );
			preFormatter.setWrapIndentBraces ( lineWrappingStyle==WRAP_DEFAULT );
			preFormatter.setWrapLineLength ( lineLength );
			preFormatter.setReplaceTabs ( replaceTabs );
			preFormatter.setNumberSpaces ( numberSpaces );
		}
	}


	void CodeGenerator::setKeyWordCase ( StringTools::KeywordCase keyCase )
	{
		keywordCase = keyCase;
	}


	void CodeGenerator::addMarkedLine ( int lineNo, string& helpTxt )
	{
		markLines[lineNo] = helpTxt;
	}


	const LanguageDefinition &CodeGenerator::getLanguage()
	{
		return langInfo;
	}


	void CodeGenerator::setEOLDelimiter(char delim){
		eolDelimiter = delim;
	}


	void CodeGenerator::reset()
	{
		lineIndex = 0;
		lineNumber = 0;
		line.clear();
		preFormatter.reset();
		inFile.clear();
		outFile.clear();
		hostLangDefPath.clear();
		embedLangDefPath.clear();
		printNewLines=true;
	}


	/** sucht vorwaerts ab Position searchPos Ziffer in s und liefert Integerwert
	der gefundenen Zahl zurueck.
	Im SymbolString stehen die den einzelnen Symbolen zugeordneten Konstanten
	immer HINTER diesen Symbolen*/
	State CodeGenerator::getState ( const string &s, unsigned int searchPos )
	{
		string::size_type pos = s.find_first_of ( "1234567890", searchPos+1 );
		if ( pos==string::npos ) return _UNKNOWN;

		string::size_type pos2 = s.find ( ' ', pos );
		int result=_UNKNOWN;
		StringTools::str2num<int> ( result, s.substr ( pos, pos2-pos ), std::dec );
		return ( State ) result;
	}


	unsigned int CodeGenerator::getLineNumber()
	{
		return lineNumber;
	}


	bool CodeGenerator::readNewLine ( string &newLine )
	{

		bool eof;
		if ( lineIndex ) terminatingChar=newLine[lineIndex-1];
		if ( formattingPossible && formattingEnabled )
		{
			eof=!formatter->hasMoreLines();
			if ( !eof )
			{
				newLine = formatter->nextLine();
			}
		}
		// reformatting not enabled
		else
		{
			eof = ! getline ( *in, newLine, eolDelimiter );
		}

		return eof || ( lineNumber == maxLineCnt );
	}


	void CodeGenerator::matchRegex ( const string &line )
	{

		regexGroups.clear();
		int matchBegin=0;
		int matchLen=0;
		int groupID=0;

		// cycle through all regex, save the start and ending indices to report them later
		for ( unsigned int i=0; i<langInfo.getRegexElements().size(); i++ )
		{
			RegexElement *regexElem = langInfo.getRegexElements() [i];
#if __cplusplus >= 201103L
			unique_ptr<Matcher> matcher ( regexElem->rePattern->createMatcher ( line ) );
#else
			auto_ptr<Matcher> matcher ( regexElem->rePattern->createMatcher ( line ) );
#endif

			while ( matcher->findNextMatch() )
			{
				groupID = ( regexElem->capturingGroup<0 ) ? matcher->getGroupNum()-1 : regexElem->capturingGroup;
				matchBegin =  matcher->getStartingIndex ( groupID );
				if ( matchBegin<0 ) continue;

				matchLen = matcher->getEndingIndex ( groupID ) - matchBegin;
				/*
				            std::cerr << "\nmatchBegin="<<1+ matchBegin
				                      << " matchLen old" << ( matcher->getGroup(matcher->getGroupNum()-1).size())
				                      << " matchLen new" << matchLen<<" group: "<<(matcher->getGroup(matcher->getGroupNum()-1))
				                      << " group id= "<<regexElem->capturingGroup
							<< " lang= "<<regexElem->langName<<"\n";
					    cerr<<"match: "<<(matcher->getGroup(matcher->getGroupNum()-1))<<" id: "<<regexElem->open<<endl;
				*/
				regexGroups.insert (
				    make_pair ( matchBegin+1, ReGroup ( regexElem->open, matchLen, regexElem->kwClass, regexElem->langName ) ) );
			}
		}
	}


	unsigned char CodeGenerator::getInputChar()
	{
		bool eol = lineIndex == line.length();

		if ( eol )
		{
			bool eof=false;
			if ( preFormatter.isEnabled() )
			{
				if ( !preFormatter.hasMoreLines() )
				{
					eof=readNewLine ( line );
					preFormatter.setLine ( line );
				}
				line = preFormatter.getNextLine();
			}
			else
			{
				eof=readNewLine ( line );
			}
			lineIndex=0;
			++lineNumber;
			//line=StringTools::trimRight ( line );
			matchRegex ( line );

			return ( eof ) ?'\0':'\n';
		}

		return line[lineIndex++];
	}


	State CodeGenerator::getCurrentState ()
	{

		unsigned char c='\0';

		if ( token.length() ==0 )
		{
			c=getInputChar();
		}
		else
		{
			lineIndex-= ( token.length()-1 );
			c=token[0];
		}
		if ( c=='\n' )
		{
			return _EOL;   // End of line
		}

		if ( c=='\0' )
		{
			return _EOF;   // End of file
		}

		if ( c==' ' || c=='\t' )
		{
			token= c;
			return _WS;
		}

		/** TODO
		    COMMENT ... END 2 eintraege in langdef (ML_COMMENT_START=regex(), ML_COMMENT_END=regex())
		    weil comment sonst als identifier erkannt wird
		*/

		// Test if a regular expression was found at the current position
		if ( !regexGroups.empty() )
		{
			if ( regexGroups.count ( lineIndex ) )
			{
				token = line.substr ( lineIndex-1, regexGroups[lineIndex].length );
				unsigned int oldIndex= lineIndex;
				if ( regexGroups[oldIndex].length>1 ) lineIndex+= regexGroups[oldIndex].length-1;


				if ( regexGroups[oldIndex].state==EMBEDDED_CODE_BEGIN) {
					embedLangDefPath = langInfo.getNewPath(regexGroups[oldIndex].name);
				}
				
				if ( regexGroups[oldIndex].state==IDENTIFIER_BEGIN || regexGroups[oldIndex].state==KEYWORD )
				{
					string reservedWord= ( langInfo.isIgnoreCase() ) ? StringTools::change_case ( token ) :token;
					currentKeywordClass=langInfo.isKeyword ( reservedWord );
					if ( !currentKeywordClass && regexGroups[oldIndex].state==KEYWORD )
						currentKeywordClass = regexGroups[oldIndex].kwClass;
					return ( currentKeywordClass ) ? KEYWORD : STANDARD;
				}
				else
				{
					return regexGroups[oldIndex].state;
				}
			}
		}

		unsigned int symbolLength;
		size_t symbolPos;
		size_t symbolFind;
		string symbols=langInfo.getSymbolString();

		//TODO this while loop kills performance - adjust search algorithm

		symbolPos = symbols.find ( c );
		// search symbols (comment delimiters, directives etc.)
		// before keywords, because alphabetic chars may be part of symbols, too
		
		while ( symbolPos!= string::npos && ! std::isdigit(c)) // isdigit: fix rexx issue
		{
			symbolFind = symbols.find ( ' ', symbolPos );
			if ( symbolFind==string::npos ) break;
			symbolLength=symbolFind-symbolPos;
			token = symbols.substr ( symbolPos, symbolLength );
			// Abfrage nach Leerzeichen in SymbolString verhindert falsches
			// Erkennen von Symbolteilen:
			if ( lineIndex && token == line.substr ( lineIndex-1, symbolLength )
			        && symbols[symbolPos-1]==' ' )
			{
				lineIndex += ( symbolLength-1 );
				return getState ( symbols, symbolPos );
			}
			else
			{
				symbolPos = symbols.find_first_not_of ( ' ',symbols.find ( ' ',symbolPos ) );
			}
		}

		// Character not referring to any state
		token = c;
		return STANDARD;
	}


	//it is faster to pass ostream reference
	void CodeGenerator::maskString ( ostream& ss, const string & s )
	{
		for ( unsigned int i=0;i< s.length();i++ )
		{
			ss << maskCharacter ( s[i] );
		}
	}


	void CodeGenerator::printMaskedToken ( bool addMetaInfo, bool flushWhiteSpace,
	                                       StringTools::KeywordCase tcase )
	{
		if ( flushWhiteSpace ) flushWs();

		if ( addMetaInfo && tagsEnabled )
		{
			bool insertMetaInfo=metaInfo.tagExists ( token );
			if ( insertMetaInfo ) *out<<getMetaInfoOpenTag ( metaInfo.getTagInfo ( token ) );
			maskString ( *out, StringTools::change_case ( token, tcase ) );
			if ( insertMetaInfo ) *out<<getMetaInfoCloseTag();
		}
		else
		{
			maskString ( *out, StringTools::change_case ( token, tcase ) );
		}
		token.clear();
	}


	bool CodeGenerator::styleFound()
	{
		return docStyle.found();
		//return docStyles.front().found();
	}


	bool CodeGenerator::printIndexFile ( const vector<string> &fileList,
	                                     const string &outPath )
	{
		return true;
	}


	bool CodeGenerator::initIndentationScheme ( const string &indentScheme )
	{

		if ( formatter!=NULL )
		{
			return true;
		}

		if ( !indentScheme.size() ) return false;

		formatter=new astyle::ASFormatter();
		
		formatter->setParensHeaderPaddingMode(true);

		if ( indentScheme=="allman" || indentScheme=="bsd" || indentScheme=="ansi" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_ALLMAN );
		}
		else if ( indentScheme=="kr"||indentScheme=="k&r"||indentScheme=="k/r" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_KandR );
		}
		else if ( indentScheme=="java" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_JAVA );
		}
		else if ( indentScheme=="stroustrup" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_STROUSTRUP );
		}
		else if ( indentScheme=="whitesmith" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_WHITESMITH );
		}
		else if ( indentScheme=="banner" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_BANNER );
		}
		else if ( indentScheme=="gnu" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_GNU );
		}
		else if ( indentScheme=="linux" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_LINUX );
		}
		else if ( indentScheme=="horstmann" )
		{
			formatter->setFormattingStyle ( astyle::STYLE_HORSTMANN );
		}
		else if ( indentScheme=="otbs" ||  indentScheme=="1tbs")
		{
			formatter->setFormattingStyle ( astyle::STYLE_1TBS );
		}
		else
		{
			return false;
		}

		return formattingEnabled=true;
	}


	LoadResult CodeGenerator::loadLanguage ( const string& langDefPath )
	{
		bool reloadNecessary= langInfo.needsReload ( langDefPath );
		if ( reloadNecessary )
		{
			//cerr<<"LOADING"<<langDefPath<<endl;
			if ( !langInfo.load ( langDefPath ) )
			{
				return langInfo.getFailedRegex().size() ? LOAD_FAILED_REGEX : LOAD_FAILED;
			}

			formattingPossible=langInfo.enableReformatting();

			if ( openTags.size() >NUMBER_BUILTIN_STATES )
			{
				// remove dynamic keyword tag delimiters of the old language definition
				vector<string>::iterator keyStyleOpenBegin =
				    openTags.begin() + NUMBER_BUILTIN_STATES;
				vector<string>::iterator keyStyleCloseBegin =
				    closeTags.begin() + NUMBER_BUILTIN_STATES;
				openTags.erase ( keyStyleOpenBegin, openTags.end() );
				closeTags.erase ( keyStyleCloseBegin, closeTags.end() );
			}
			// add new keyword tag delimiters
			for ( unsigned int i=0;i< langInfo.getKeywordClasses().size(); i++ )
			{
				openTags.push_back ( getKeywordOpenTag ( i ) );
				closeTags.push_back ( getKeywordCloseTag ( i ) );
			}
		}
		return ( reloadNecessary) ? LOAD_NEW : LOAD_NONE;
	}


	bool CodeGenerator::initTagInformation ( const string& ctagsPath )
	{
		if ( tagsEnabled ) return true; // load tag info once
		tagsEnabled = metaInfo.load ( ctagsPath );
		return tagsEnabled;
	}


	bool CodeGenerator::validateInputStream()
	{
		if ( !in ) return false;

		// it is not possible to move stream pointer back with stdin
		if ( ( int ) in->tellg() == -1 ) // -1 : stdin
			return true;

		// Sources: http://en.wikipedia.org/wiki/Magic_number_(programming)
		// Magic configuration of "file"
		// This is intended for web plugins - only check filetypes often found in the net
		char magic_gif[]    = {'G','I','F','8', 0};
		char magic_png[]    = {(char)0x89,'P','N','G', 0};
		char magic_java[]   = {(char)0xCA,(char)0xFE,(char)0xBA,(char)0xBE, 0};
		char magic_jpeg[]   = {(char)0xFF,(char)0xD8,(char)0xFF, 0};
		char magic_bmp[]    = {'B','M', 0};
		char magic_pdf[]    = {'%','P','D','F', 0};
		char magic_utf8[]   = {(char)0xEF,(char)0xBB,(char)0xBF,0};
		char magic_rar[]    = {'R','a','r','!', 0};
		char magic_zip[]    = {'P','K',(char)0x03,(char)0x04, 0};
		char magic_ace[]    = {'*','*','A','C','E','*','*', 0};
		char magic_tgz[]    = {(char)0x8b,(char)0x1f, (char)0x00, (char)0x08, 0};
		char magic_bzip[]   = {'B','Z', 0};

		char* magic_table[] = {magic_utf8,
		                       magic_gif, magic_png, magic_jpeg, magic_bmp, magic_pdf,
		                       magic_java,
		                       magic_rar, magic_zip, magic_ace, magic_tgz, magic_bzip,
		                       0
		                      };

		char buffer [10]={0};
		in->read ( buffer,8 );  //only read the first 8 bytes of input stream

		int magic_index=0;
		while ( magic_table[magic_index] )
		{
			if ( !strncmp ( buffer, magic_table[magic_index], strlen ( magic_table[magic_index] ) ) )
			{
				break;
			}
			magic_index++;
		}
		int streamReadPos=0;
		if ( magic_table[magic_index] == magic_utf8 )
		{
			//setEncoding("utf-8");
			streamReadPos=3; // remove UTF-8 magic number from output
		}

		in -> seekg ( streamReadPos, ios::beg );
		in-> clear();  // clear fail bit to continue reading

		return !magic_table[magic_index] // points to 0 if no pattern was found
		       || magic_table[magic_index] == magic_utf8;
	}


	ParseError CodeGenerator::generateFile ( const string &inFileName,
	        const string &outFileName )
	{
		if ( !docStyle.found() )
		{
			return BAD_STYLE;
		}

		reset();

		ParseError error=PARSE_OK;

		inFile=inFileName;
		outFile=outFileName;
		in = ( inFileName.empty() ? &cin :new ifstream ( inFileName.c_str() ) );

		if ( validateInput )
			if ( !validateInputStream() ) error= BAD_INPUT;

		if ( !in->fail() && error==PARSE_OK )
		{
			//// out = ( outFileName.empty() ? &cout :new ofstream ( outFileName.c_str() ) );
			out = new ofstream ( outFileName.c_str() );
			if ( out->fail() )
			{
				error=BAD_OUTPUT;
			}
		}

		if ( in->fail() )
		{
			error=BAD_INPUT;
		}

		if ( error==PARSE_OK )
		{
			if ( formatter != NULL )
			{
				formatter->init ( new astyle::ASStreamIterator ( in ) );
			}
			if ( ! fragmentOutput )
			{
				*out << getHeader();
			}

			printBody();

			if ( ! fragmentOutput )
			{
				*out << getFooter();
			}
		}

		if ( !outFileName.empty() )
		{
			delete out; out=NULL;
		}
		if ( !inFileName.empty() )
		{
			delete in; in=NULL;
		}
		return error;
	}


	string CodeGenerator::generateString ( const string &input )
	{

		if ( !docStyle.found() )
		{
			return "";
		}

		reset();

		in = new istringstream ( input );
		out = new ostringstream ();

		if ( in->fail() || out->fail() )
		{
			return "";
		}

		if ( formatter != NULL )
		{
			formatter->init ( new astyle::ASStreamIterator ( in ) );
		}
		if ( ! fragmentOutput )
		{
			*out << getHeader();
		}

		printBody();

		if ( ! fragmentOutput )
		{
			*out << getFooter();
		}

		string result = static_cast<ostringstream*> ( out )->str();

		delete out; out=NULL;
		delete in; in=NULL;

		return result;
	}


	string CodeGenerator::generateStringFromFile ( const string &inFileName )
	{

		if ( !docStyle.found() )
		{
			return "";
		}

		reset();

		inFile = inFileName;
		in = new ifstream ( inFileName.c_str() );
		out = new ostringstream ();

		if ( in->fail() || out->fail() )
		{
			return "";
		}

		if ( validateInput && !validateInputStream() )
		{
			return "ERROR: detected binary input";
		}

		if ( formatter != NULL )
		{
			formatter->init ( new astyle::ASStreamIterator ( in ) );
		}
		if ( ! fragmentOutput )
		{
			*out << getHeader();
		}

		printBody();

		if ( ! fragmentOutput )
		{
			*out << getFooter();
		}

		string result = static_cast<ostringstream*> ( out )->str();

		delete out; out=NULL;
		delete in; in=NULL;

		return result;
	}


	unsigned int CodeGenerator::getStyleID ( State s, unsigned int kwClassID )
	{
		if ( s==KEYWORD && kwClassID )
		{
			return NUMBER_BUILTIN_STATES + kwClassID-1;
		}
		return ( unsigned int ) s ;
	}


	void CodeGenerator::openTag ( State s )
	{
		*out << openTags[ ( unsigned int ) s];
		currentState=s;
	}


	void CodeGenerator::closeTag ( State s )
	{
		*out << closeTags[ ( unsigned int ) s];
		flushWs();
		currentState=_UNKNOWN;
	}


	void CodeGenerator::openKWTag ( unsigned int kwClassID )
	{
		*out << openTags.at(getStyleID ( KEYWORD, kwClassID ) );
		currentState=KEYWORD;
	}


	void CodeGenerator::closeKWTag ( unsigned int kwClassID )
	{
		*out << closeTags.at(getStyleID ( KEYWORD, kwClassID ) );
		flushWs();
		currentState=_UNKNOWN;
	}

	void CodeGenerator::loadEmbeddedLang(const string&embedLangDefPath){
			//save path of host language
			if (hostLangDefPath.empty()) {
				hostLangDefPath =langInfo.getCurrentPath();
			}
			//load syntax of embedded langage
			loadLanguage(embedLangDefPath);
			//pass end delimiter regex to syntax description
			langInfo.restoreLangEndDelim(embedLangDefPath);
	}

///////////////////////////////////////////////////////////////////////////////

	void CodeGenerator::processRootState()
	{

		bool eof=false,
		         firstLine=true; // avoid newline before printing the first output line

		if ( langInfo.highlightingDisabled() )
		{
			string line;
			while ( getline ( *in, line ) )
			{
				++lineNumber;
				insertLineNumber ( !firstLine );
				flushWs();
				firstLine=false;
				maskString ( *out, line );
			}
			*out << flush;
			return;
		}

		if (!embedLangStart.empty()) loadEmbeddedLang(langInfo.getNewPath(embedLangStart));

		State state=STANDARD;

		openTag ( STANDARD );
		do
		{
			// determine next state
			state= getCurrentState();

			// handle current state
			switch ( state )
			{
				case KEYWORD:
					closeTag ( STANDARD );
					eof=processKeywordState ( state );
					openTag ( STANDARD );
					break;
				case NUMBER:
					closeTag ( STANDARD );
					eof=processNumberState();
					openTag ( STANDARD );
					break;
				case ML_COMMENT:
					closeTag ( STANDARD );
					eof=processMultiLineCommentState();
					openTag ( STANDARD );
					break;
				case SL_COMMENT:
					closeTag ( STANDARD );
					eof=processSingleLineCommentState();
					openTag ( STANDARD );
					break;
				case STRING:
					closeTag ( STANDARD );
					eof=processStringState ( STANDARD );
					openTag ( STANDARD );
					break;
				case DIRECTIVE:
					closeTag ( STANDARD );
					eof=processDirectiveState();
					openTag ( STANDARD );
					break;
				case ESC_CHAR:
					if ( langInfo.allowExtEscSeq() )
					{
						closeTag ( STANDARD );
						eof=processEscapeCharState();
						openTag ( STANDARD );
					}
					else
					{
						printMaskedToken();
					}
					break;
				case SYMBOL:
					closeTag ( STANDARD );
					eof=processSymbolState();
					openTag ( STANDARD );
					break;
				case EMBEDDED_CODE_BEGIN:
				case EMBEDDED_CODE_END:
					closeTag ( STANDARD );
					eof=processSyntaxChangeState(state);
					openTag ( STANDARD );
					break;
				case _EOL:
					insertLineNumber ( !firstLine );
					firstLine=false;
					break;
				case _EOF:
					eof=true;
					break;
				case _WS:
					processWsState();
					break;
				default:
					printMaskedToken ( true );
					break;
			}
		}
		while ( !eof );
		closeTag ( STANDARD );
		printNewLines = !noTrailingNewLine;
		*out << getNewLine();
		*out << flush;
	}

	bool CodeGenerator::processSyntaxChangeState(State myState)
	{
		State newState=STANDARD;
		bool eof=false,
		         exitState=false;
		openTag ( KEYWORD );
		do
		{
			if (myState==EMBEDDED_CODE_BEGIN) {
				loadEmbeddedLang(embedLangDefPath);
				
				//test current line again to match tokens of the embedded language
				matchRegex(line);
				
			}
			else if (myState==EMBEDDED_CODE_END) {
				// load host language syntax
				loadLanguage(hostLangDefPath);
				//test current line again to match tokens of the host language
				matchRegex(line);
				
			}

			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();
			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					insertLineNumber();
					exitState=true;
					break;
				case _EOF:
					eof = true;
					break;
				default:
					exitState=true;
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( KEYWORD );
		return eof;
	}


	bool CodeGenerator::processKeywordState ( State myState )
	{
		State newState=STANDARD;
		unsigned int myClassID=currentKeywordClass;
		bool eof=false,
		         exitState=false;

		openKWTag ( myClassID );
		do
		{
			printMaskedToken ( true, newState!=_WS,
			                   ( langInfo.isIgnoreCase() ) ? keywordCase : StringTools::CASE_UNCHANGED );
			newState= getCurrentState();
			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					insertLineNumber();
					exitState=true;
					break;
				case _EOF:
					eof = true;
					break;
				case KEYWORD_END:
					exitState=true;
					break;
				default:
					exitState= ( myClassID!=currentKeywordClass ) || ( myState!=newState );
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeKWTag ( myClassID );

		currentKeywordClass=0;
		return eof;
	}


	bool CodeGenerator::processNumberState()
	{
		State newState=STANDARD;
		bool eof=false,
		         exitState=false;
		openTag ( NUMBER );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();
			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					insertLineNumber();
					exitState=true;
					break;
				case _EOF:
					eof = true;
					break;
				default:
					exitState=newState!=NUMBER;
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( NUMBER );
		return eof;
	}


	bool CodeGenerator::processMultiLineCommentState()
	{
		int commentCount=1;
		int delimPairID = langInfo.getDelimiterPairID ( token );
		State newState=STANDARD;
		bool eof=false, exitState=false;
		openTag ( ML_COMMENT );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();

			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					wsBuffer += closeTags[ML_COMMENT];
					insertLineNumber();
					wsBuffer += openTags[ML_COMMENT];
					break;
				case _EOF:
					eof = true;
					break;
				case ML_COMMENT:
					if ( langInfo.allowNestedMLComments() )
					{
						++commentCount;
					}
					// if delimiters are equal, close the comment by continueing to
					// ML_COMMENT_END section
					if ( langInfo.delimiterIsDistinct ( ML_COMMENT ) )  break;

				case ML_COMMENT_END:
					if ( delimPairID!=langInfo.getDelimiterPairID ( token ) ) break;
					commentCount--;
					if ( !commentCount )
					{
						printMaskedToken();
						exitState=true;
					}
					break;
				default:
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( ML_COMMENT );
		return eof;
	}


	bool CodeGenerator::processSingleLineCommentState()
	{

		if ( checkSpecialCmd() )
		{
			return in->bad(); // if input stream is bad, report eof to calling method
		}

		State newState=STANDARD;
		bool eof=false, exitState=false;

		openTag ( SL_COMMENT );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();

			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					printMaskedToken();
					if ( preFormatter.isEnabled() && preFormatter.isWrappedLine ( lineNumber-1 ) )
					{
						exitState=false;
					}
					else
					{
						exitState=true;
					}
					if ( !exitState ) wsBuffer += closeTags[SL_COMMENT];
					insertLineNumber();
					if ( !exitState ) wsBuffer += openTags[SL_COMMENT];

					break;
				case _EOF:
					eof = true;
					break;
				default:
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( SL_COMMENT );
		return eof;
	}


	bool CodeGenerator::processDirectiveState()
	{
		State  newState=STANDARD;
		bool eof=false, exitState=false;

		openTag ( DIRECTIVE );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();
			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case DIRECTIVE_END:
					printMaskedToken();
					exitState=true;
					break;
				case _EOL:
					printMaskedToken();
					if ( preFormatter.isEnabled() && preFormatter.isWrappedLine ( lineNumber-1 ) )
					{
						exitState=false;
					}
					else
					{
						exitState= ( terminatingChar!=langInfo.getContinuationChar() );
					}
					if ( !exitState ) wsBuffer += closeTags[DIRECTIVE];
					insertLineNumber();
					if ( !exitState ) wsBuffer += openTags[DIRECTIVE];
					break;
				case ML_COMMENT:
					closeTag ( DIRECTIVE );
					eof= processMultiLineCommentState();
					openTag ( DIRECTIVE );
					break;
				case SL_COMMENT:
					closeTag ( DIRECTIVE );
					eof= processSingleLineCommentState();
					openTag ( DIRECTIVE );
					exitState=true;
					break;
				case STRING:
					closeTag ( DIRECTIVE );
					eof=processStringState ( DIRECTIVE );
					openTag ( DIRECTIVE );
					break;
				case _EOF:
					eof = true;
					break;
				default:
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( DIRECTIVE );
		return eof;
	}


	bool CodeGenerator::processStringState ( State oldState )
	{
		State newState=STANDARD;
		bool eof=false, exitState=false;
		bool returnedFromOtherState=false;
		// Test if character before string open delimiter token equals to the
		// raw string prefix (Example: r" ", r""" """ in Python)
		bool isRawString=false;
		if ( lineIndex>token.length() )
		{
			isRawString = line[lineIndex-token.length()-1]==langInfo.getRawStringPrefix();
		}
		int delimPairID = langInfo.getDelimiterPairID ( token );
		State myState= ( oldState==DIRECTIVE ) ? DIRECTIVE_STRING : STRING;
		openTag ( myState );
		do
		{
			// true if last token was an escape char
			if ( !returnedFromOtherState )
			{
				printMaskedToken ( false, newState!=_WS );
			}
			returnedFromOtherState=false;
			newState= getCurrentState();

			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					wsBuffer += closeTags[myState];
					insertLineNumber();
					wsBuffer += openTags[myState];
					break;
				case STRING_END:
					exitState= true;
					printMaskedToken();
					break;
				case STRING:
					// if there exist multiple string delimiters, close string if
					// current delimiters is equal to the opening delimiter
					exitState = ( delimPairID==langInfo.getDelimiterPairID ( token ) );
					printMaskedToken();
					break;
				case ESC_CHAR:
					if ( !isRawString )
					{
						closeTag ( myState );
						eof=processEscapeCharState();
						openTag ( myState );
						returnedFromOtherState=true;
					}
					break;
				case _EOF:
					eof = true;
					break;
				default:
					printMaskedToken();
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( myState );
		return eof;
	}


	bool CodeGenerator::processSymbolState()
	{

		State newState=STANDARD;
		bool eof=false,
		         exitState=false;

		openTag ( SYMBOL );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();
			switch ( newState )
			{
				case _WS:
					processWsState();
					break;
				case _EOL:
					insertLineNumber();
					exitState=true;
					break;
				case _EOF:
					eof = true;
					break;
				default:
					exitState=newState!=SYMBOL;
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( SYMBOL );
		return eof;
	}


	bool CodeGenerator::processEscapeCharState()
	{
		State newState=STANDARD;
		bool eof=false, exitState=false;
		openTag ( ESC_CHAR );
		do
		{
			printMaskedToken ( false, newState!=_WS );
			newState= getCurrentState();
			switch ( newState )
			{
				case _EOL:
					insertLineNumber();
					exitState=true;
					break;
				case _WS:
					processWsState();
					break;
				case _EOF:
					eof = true;
					break;
				default:
					exitState=newState!=ESC_CHAR;
					break;
			}
		}
		while ( ( !exitState ) && ( !eof ) );

		closeTag ( ESC_CHAR );
		return eof;
	}


	void CodeGenerator::processWsState()
	{
		if ( !maskWs )
		{
			wsBuffer += token;
			token.clear();
			return;
		}
		flushWs();
		int cntWs=0;
		lineIndex--;

		// while (iswspace(line[lineIndex])  ) {
		while ( line[lineIndex]==' ' || line[lineIndex]=='\t' )
		{
			++cntWs;
			++lineIndex;
		}

		if ( cntWs>1 )
		{
			unsigned int styleID=getStyleID ( currentState, currentKeywordClass );
			if ( excludeWs && styleID!=_UNKNOWN )
			{
				*out << closeTags[styleID];
			}
			*out << maskWsBegin;
			for ( int i=0; i<cntWs; i++ )
			{
				*out << spacer;
			}
			*out << maskWsEnd;
			if ( excludeWs && styleID!=_UNKNOWN )
			{
				*out << openTags[styleID];
			}
		}
		else
		{
			*out << spacer; //Bugfix fehlender Space nach Strings
		}
		token.clear();
	}


	void CodeGenerator::flushWs()
	{
		*out<<wsBuffer;
		wsBuffer.clear();
	}


	string CodeGenerator::getNewLine()
	{
		return (printNewLines) ? newLineTag : "";
	}


	void CodeGenerator::insertLineNumber ( bool insertNewLine )
	{

		if ( insertNewLine )
		{
			wsBuffer += getNewLine();
		}

		if ( showLineNumbers )
		{
			ostringstream os;
			ostringstream numberPrefix;
			if ( lineNumberFillZeroes )
			{
				os.fill ( '0' );
			}
			os <<setw ( getLineNumberWidth() ) << right << lineNumber+lineNumberOffset;

			numberPrefix << openTags[LINENUMBER];
			maskString ( numberPrefix, os.str() );
			numberPrefix << spacer
			<< closeTags[LINENUMBER];

			wsBuffer += numberPrefix.str();
		}
	}


	unsigned int CodeGenerator::getLineIndex()
	{
		return lineIndex;
	}


	bool CodeGenerator::printExternalStyle ( const string &outFile )
	{
		if ( !includeStyleDef && langInfo.highlightingEnabled() )
		{
			//// ostream *cssOutFile = ( outFile.empty() ? &cout :new ofstream ( outFile.c_str() ) );
			ostream *cssOutFile = new ofstream ( outFile.c_str() );
			if ( !cssOutFile->fail() )
			{
				*cssOutFile << styleCommentOpen
				<<" Style definition file generated by highlight "
				<< HIGHLIGHT_VERSION << ", " << HIGHLIGHT_URL
				<< " " << styleCommentClose << "\n";
				*cssOutFile << "\n" << styleCommentOpen
				<< " Highlighting theme definition: "
				<< styleCommentClose << "\n\n"
				<< getStyleDefinition()
				<< "\n";
				*cssOutFile << readUserStyleDef();
				if ( !outFile.empty() ) delete cssOutFile;
			}
			else
			{
				return false;
			}
		}
		return true;
	}


	string CodeGenerator::readUserStyleDef()
	{
		ostringstream ostr;
		if ( !styleInputPath.empty() )
		{
			ifstream userStyleDef ( styleInputPath.c_str() );
			if ( userStyleDef )
			{
				ostr 	<< "\n" << styleCommentOpen
					<< " Content of " << styleInputPath
					<< ": " <<styleCommentClose << "\n";
				string line;
				while ( getline ( userStyleDef, line ) )
				{
					ostr << line << "\n";
				}
				userStyleDef.close();
			}
			else
			{
				ostr 	<< styleCommentOpen
					<< " ERROR: Could not include " << styleInputPath
					<< "." << styleCommentClose << "\n";
			}
		}
		return ostr.str();
	}


	bool CodeGenerator::checkSpecialCmd()
	{

		//cerr << "token: "<<token<< " index"<< lineIndex << " "<<line [ lineIndex ]<<  "sizes: "<<token.size()<<"=="<<line.size()<<endl;
		string noParseCmd="@highlight";
		// if single line comment is described with regex, token is equal to line
		// otherwise start searching after the token, which then consists of comment identifier
		size_t searchStart= ( token.size() ==line.size() ) ? 0 : lineIndex;
		size_t cmdPos = line.find ( noParseCmd, searchStart );
		size_t pos=1;
		if ( cmdPos!=string::npos )
		{
			string res;
			string replaceVar;

#if __cplusplus >= 201103L
			unique_ptr<Pattern> reDefPattern ( Pattern::compile ( "\\$[-\\w]+" ) );
			unique_ptr<Matcher> m ( reDefPattern->createMatcher ( line.substr ( noParseCmd.size() +cmdPos ) ) );
#else
			auto_ptr<Pattern> reDefPattern ( Pattern::compile ( "\\$[-\\w]+" ) );
			auto_ptr<Matcher> m ( reDefPattern->createMatcher ( line.substr ( noParseCmd.size() +cmdPos ) ) );
#endif
			while ( m.get() &&  m->findNextMatch() )
			{
				res+=line.substr ( noParseCmd.size() +cmdPos + pos ,
				                   m->getStartingIndex ( 0 )-pos );
				replaceVar = m->getGroup ( 0 );
				if ( replaceVar=="$nl" )
				{
					res+="\n";
				}
				else if ( replaceVar=="$infile" )
				{
					res+= ( inFile.size() ) ? inFile: "stdin";
				}
				else if ( replaceVar=="$outfile" )
				{
					res+= ( outFile.size() ) ? outFile: "stdout";
				}
				else if ( replaceVar=="$title" )
				{
					res+= docTitle;
				}
				else if ( replaceVar=="$theme"||replaceVar=="$style" )
				{
					res+= getStyleName();
				}
				else if ( replaceVar=="$font-face" )
				{
					res+= getBaseFont();
				}
				else if ( replaceVar=="$font-size" )
				{
					res+= getBaseFontSize();
				}
				else if ( replaceVar=="$encoding" )
				{
					res+= encoding;
				}
				else if ( replaceVar=="$linenum" )
				{
					char numBuf[10];
					sprintf ( numBuf, "%d", lineNumber );
					res+= string ( numBuf );
				}
				pos=m->getEndingIndex ( 0 );
			}
			res+=line.substr ( noParseCmd.size() +cmdPos + pos );

			*out<<res;

			// hide comment line from output
			token.clear();
			lineIndex=line.length();
			getInputChar();
			lineNumber--;
			// end hide

			return true; // do not parse line as comment
		}

		return false; //parse comment as usual
	}

}
