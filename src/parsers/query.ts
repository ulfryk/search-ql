import { OrderedMap } from 'immutable';
import { None, Some } from 'monet';
import * as P from 'parsimmon';

import { Expression, fromPairs, NotExpression } from '../expressions';
import { GROUP_END, GROUP_START, NOT } from '../syntax-config';
import { basicExpression } from './basic';
import { labelledExpression } from './labelled';
import { logicalOperator } from './logical-operator';
import { ParserName } from './names';

const parserMappings = OrderedMap<ParserName, (parserNames: ParserName[]) => P.Parser<any>>([
  // tslint:disable-next-line:no-use-before-declare no-unnecessary-callback-wrapper
  [ParserName.JoinedGroup, (parserNames: ParserName[]) => joinedGroup(parserNames)],
  [ParserName.Labelled, () => labelledExpression],
  // tslint:disable-next-line:no-use-before-declare no-unnecessary-callback-wrapper
  [ParserName.Not, (parserNames: ParserName[]) => notExpression(parserNames)],
  [ParserName.Basic, () => basicExpression],
]);

const getParsers = (parserNames: ParserName[]): P.Parser<any>[] =>
  parserMappings
    .filter((__, name) => parserNames.includes(name))
    .map(getParser => getParser(parserNames))
    .toArray();

const subQuery = (parserNames: ParserName[]): P.Parser<Expression> =>
  P.lazy(() => P.alt(...getParsers(parserNames)));

const operator = P.whitespace.then(logicalOperator).skip(P.whitespace);

const queryLogicalPart = (parserNames: ParserName[]) =>
  P.seqMap(operator, subQuery(parserNames), (op, expression) => [Some(op), expression]);

const joinedExpression = (parserNames: ParserName[]): P.Parser<Expression> => P.seqMap(
  subQuery(parserNames),
  queryLogicalPart(parserNames).many(),
  (head, tail) => [[None<string>(), head]].concat(tail),
).map(fromPairs);

const joinedGroup = (parserNames: ParserName[]) => P.seqMap(
  P.string(GROUP_START).skip(P.optWhitespace),
  joinedExpression(parserNames),
  P.optWhitespace.then(P.string(GROUP_END)),
  (_start, logical, _end) => logical);

const notExpression = (parserNames: ParserName[]): P.Parser<Expression> => P.seqMap(
  P.string(NOT).skip(P.whitespace),
  subQuery(parserNames),
  (_operator, logical) => new NotExpression(logical),
);

const query = (parserNames: ParserName[]): P.Parser<Expression> =>
  P.alt(joinedExpression(parserNames), subQuery(parserNames));

export { query };
