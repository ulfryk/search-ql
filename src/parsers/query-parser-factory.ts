import { OrderedMap } from 'immutable';
import * as P from 'parsimmon';

import { fromPairs, FunctionExpression, initialPair, NotExpression, OOPair, restPair } from '../ast';
import { Expression, ReshapeContext } from '../common/model';
import { ParserConfig, ParserName } from '../config';
import { binaryOperator } from './binary-operator';
import { termExpression } from './term';
import { unaryOperator } from './unary-operator';

export class QueryParserFactory {

  private readonly parserMappings = OrderedMap<ParserName, () => P.Parser<any>>([
    [ParserName.Function, () => this.function],
    [ParserName.BinaryOperation, () => this.groupedBinaryOperation],
    [ParserName.Not, () => this.notExpression],
    [ParserName.Basic, () => termExpression(this.config)],
  ]);

  private readonly binaryOperator = P.whitespace
    .then(binaryOperator(this.config))
    .skip(P.whitespace);

  constructor(
    public readonly config: ParserConfig = new ParserConfig(),
  ) {}

  public getParser(): P.Parser<Expression> {
    return (this.config.parserNames.includes(ParserName.BinaryOperation) ?
        P.alt(this.binaryOperation, this.query) :
        this.query)
      .map(ast => ast.reshape(ReshapeContext.Top));
  }

  private getParsers(): P.Parser<any>[] {
    return this.parserMappings
      .filter((__, name) => this.config.parserNames.includes(name))
      .map(getParser => getParser())
      .toArray();
  }

  private get query(): P.Parser<Expression> {
    return P.lazy(() => P.alt(...this.getParsers()));
  }

  private get binaryOperation(): P.Parser<Expression> {
    return P.seqMap(
      this.query.map(initialPair),
      this.rightHandSide.many(),
      (leftOperand, operatorAndRightOperandExpressions) => [
        leftOperand,
        ...operatorAndRightOperandExpressions,
      ],
    ).map(parser => fromPairs(parser, this.config));
  }

  private get rightHandSide(): P.Parser<OOPair> {
    return P.seqMap(this.binaryOperator, this.query, restPair);
  }

  // TODO: DROP IT, DROP, make anything grouped, right ?
  private get groupedBinaryOperation(): P.Parser<Expression> {
    const { GROUP_END, GROUP_START } = this.config;

    return this.binaryOperation.wrap(this.leftWrap(GROUP_START), this.rightWrap(GROUP_END));
  }

  // TODO: change to unaryOperationExpression
  private get notExpression(): P.Parser<Expression> {
    // TODO: try lookahead
    return P.seqMap(
      unaryOperator(this.config).skip(P.whitespace),
      this.query,
      NotExpression.fromParseResult);
  }

  private get function(): P.Parser<Expression> {
    return P.seqMap(
      this.functionName,
      this.functionArgs,
      (name: string, args: Expression[]) =>
        FunctionExpression.fromParseResult(this.config.functions.get(name), args));
  }

  private get functionArgs(): P.Parser<Expression[]> {
    const { FN_ARG_SEPARATOR, FN_LEFT_PAREN, FN_RIGHT_PAREN } = this.config;

    return this.query
      .sepBy(P.string(FN_ARG_SEPARATOR).wrap(P.optWhitespace, P.optWhitespace))
      .wrap(this.leftWrap(FN_LEFT_PAREN), this.rightWrap(FN_RIGHT_PAREN));
  }

  private get functionName(): P.Parser<string> {
    const validNames = this.config.functions.keySeq().toArray();

    return P.alt(...validNames.map(name => P.string(name)));
  }

  private leftWrap(token: string) {
    return P.string(token).skip(P.optWhitespace);
  }

  private rightWrap(token: string) {
    return P.optWhitespace.then(P.string(token));
  }

}
