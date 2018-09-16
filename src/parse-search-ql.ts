import { Either, Left, Right } from 'monet';
import { Failure as ParsimmonFailure } from 'parsimmon';

import { validate } from './ast';
import { Expression, Failure, ParseFailure } from './common/model';
import { ParserConfig } from './config';
import { QueryParserFactory } from './parsers';

export const parseSearchQL = (config?: Partial<ParserConfig>) => {
  const factory = new QueryParserFactory(ParserConfig.create(config));

  return (query: string): Either<Failure[], Expression> => {
    const parsed = factory.getParser().parse(query);

    return parsed.status ?
      Right<Failure[], Expression>(parsed.value).flatMap(validate) :
      Left<Failure[], Expression>([ParseFailure.fromFailure(parsed as ParsimmonFailure, query)]);
  };
};
