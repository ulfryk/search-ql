import { Either, Left, Right } from 'monet';
import { Failure as ParsimmonFailure } from 'parsimmon';

import { validate } from './ast';
import { Expression, Failure, ParseFailure } from './common/model';
import { SyntaxConfig } from './config';
import { ParserName, QueryParserFactory } from './parsers';

export const parseSearchQL = (parserNames: ParserName[], config?: SyntaxConfig) => {
  const queryParserConfigured = new QueryParserFactory(parserNames, config).getParser();

  return (query: string): Either<Failure[], Expression> => {
    const parsed = queryParserConfigured.parse(query);

    return parsed.status ?
      Right<Failure[], Expression>(parsed.value.reshape()).flatMap(validate) :
      Left<Failure[], Expression>([ParseFailure.fromFailure(parsed as ParsimmonFailure, query)]);
  };
};
