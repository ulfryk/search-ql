import { Either, Left, Right } from 'monet';
import { Failure } from 'parsimmon';

import { Expression } from './expressions';
import { ParseFailure } from './parse-failure';
import { ParserName, query as queryParser } from './parsers';

export const parseSearchQL = (parserNames: ParserName[]) => {
  const queryParserConfigured = queryParser(parserNames);

  return (query: string): Either<ParseFailure, Expression> => {
    const parsed = queryParserConfigured.parse(query);

    return parsed.status ?
      Right<ParseFailure, Expression>(parsed.value) :
      Left<ParseFailure, Expression>(ParseFailure.fromFailure(parsed as Failure, query));
  };
};
