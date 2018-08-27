import { OrderedMap } from 'immutable';
import { Maybe, None, Some } from 'monet';
import * as P from 'parsimmon';

import { Expression, fromPairs, NotExpression } from '../ast';
import { SyntaxConfig } from '../config';
import { basicExpression } from './basic';
import { binaryOperator } from './binary-operator';
import { labelledExpression } from './labelled';
import { ParserName } from './names';

export class QueryParserFactory {

  private readonly parserMappings = OrderedMap<ParserName, () => P.Parser<any>>([
    [ParserName.JoinedGroup, () => this.groupedBinaryOperation],
    [ParserName.Labelled, () => labelledExpression(this.config)],
    [ParserName.Not, () => this.notExpression],
    [ParserName.Basic, () => basicExpression(this.config)],
  ]);

  private readonly binaryOperator = P.whitespace
    .then(binaryOperator(this.config))
    .skip(P.whitespace);

  constructor(
    public readonly parserNames: ParserName[],
    public readonly config: SyntaxConfig = new SyntaxConfig(),
  ) {}

  public getParser(): P.Parser<Expression> {
    return P.alt(this.binaryOperation, this.subQuery);
  }

  private getParsers(): P.Parser<any>[] {
    return this.parserMappings
      .filter((__, name) => this.parserNames.includes(name))
      .map(getParser => getParser())
      .toArray();
  }

  private get subQuery(): P.Parser<Expression> {
    return P.lazy(() => P.alt(...this.getParsers()));
  }

  private get rightHandSide() {
    return P.seqMap(
      this.binaryOperator,
      this.subQuery,
      (operator, expression) => [Some(operator), expression]);
  }

  private get binaryOperation(): P.Parser<Expression> {
    return P.seqMap(
      this.subQuery,
      this.rightHandSide.many(),
      (leftOperand, operatorAndRightOperandExpression) =>
        [[None<string>(), leftOperand]].concat(operatorAndRightOperandExpression),
    ).map((parser: [Maybe<string>, Expression][]) => fromPairs(parser, this.config));
  }

  private get groupedBinaryOperation(): P.Parser<Expression> {
    return P.seqMap(
      P.string(this.config.GROUP_START).skip(P.optWhitespace),
      this.binaryOperation,
      P.optWhitespace.then(P.string(this.config.GROUP_END)),
      (_leftBracket, operation, _rightBracket) => operation);
  }

  private get notExpression(): P.Parser<Expression> {
    return P.seqMap(
      P.string(this.config.NOT).skip(P.whitespace),
      this.subQuery,
      (_operator, operand) => new NotExpression(operand));
  }

}
