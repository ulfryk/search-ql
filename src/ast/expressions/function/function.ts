import { List } from 'immutable';
import { zip } from 'lodash';
import { Maybe, Some } from 'monet';

import { Expression, isSubtype, ValueType } from '../../../common/model';
import { toOrdinal } from '../../../common/utils';
import { FunctionConfig, RequiredFunctionArg } from '../../../config';
import { InvalidExpression } from '../invalid';
import { DateExpression, NumberExpression, TermExpression, TextExpression } from '../term';

export class FunctionExpression<R> extends Expression {

  public static fromParseResult<O>(config: FunctionConfig<O>, args: Expression[]) {
    const argsCount = args.length;
    const baseArgsCount = config.args.size;
    const restArgsCount = argsCount - baseArgsCount;
    const absRestCount = restArgsCount > 0 ? restArgsCount : 0;
    const argExpressions = List(config.args)
      .map(({ type }) => Some(type))
      .concat(Array(absRestCount).fill(config.argsRest.map(({ type }) => type)))
      .filter((_type, index) => Boolean(args[index]))
      .map((type, index) => type
        // tslint:disable-next-line:cyclomatic-complexity
        .foldLeft(args[index])((arg, someType) => {
          if (arg.is(TermExpression as any)) {
            switch (someType) {
              case ValueType.Date:
                return DateExpression.fromTerm(arg as TermExpression);
              case ValueType.Number:
                return NumberExpression.fromTerm(arg as TermExpression);
              case ValueType.Text:
                return TextExpression.fromTerm(arg as TermExpression);
              case ValueType.Any:
              case ValueType.Boolean:
              default:
                return arg;
            }
          }
          return arg;
        }))
      .toList();

    return new FunctionExpression(argExpressions, config);
  }

  constructor(
    public readonly value: List<Expression>,
    public readonly config: FunctionConfig<R>,
  ) { super(); }

  public get name() {
    return this.config.name;
  }

  public get returnType() {
    return this.config.returnType;
  }

  // tslint:disable-next-line:cyclomatic-complexity
  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof FunctionExpression &&
      this.name === other.name &&
      this.returnType === other.returnType &&
      this.value.equals(other.value)
    );
  }

  public isValid() {
    return this.value.every(operand => operand.isValid());
  }

  public reshape() {
    return this.clone(this.value.map(arg => arg.reshape()).toList());
  }

  public checkTypes() {
    return this.getErrors()
      .foldLeft(this.clone(this.value.map(arg => arg.checkTypes()).toList()))(
        InvalidExpression.fromErrors);
  }

  public toString() {
    return `${this.name}( ${this.value.join(' , ')} )`;
  }

  public toList() {
    return List([this])
      .concat(this.value.flatMap(operand => operand.toList()))
      .toList();
  }

  private clone(args: List<Expression>): Expression {
    if (args.equals(this.value)) {
      return this;
    }

    return new FunctionExpression(args, this.config);
  }

  private getErrors(): Maybe<string[]> {
    const restArgs = this.value.slice(this.config.args.size).toList();
    const initialArgs = this.value.slice(0, this.config.args.size).toList();
    const requiredArgsCount = this.config.args
      .filter(arg => arg instanceof RequiredFunctionArg)
      .size;

    return Some([
      ...this.getTooMuchArgsErrors(restArgs),
      ...this.getToFewArgsErrors(initialArgs.size, requiredArgsCount),
      ...this.getInitialArgsErrors(initialArgs),
      ...this.getRestArgsErrors(restArgs),
    ]).filter(errors => errors.length > 0);
  }

  private getTooMuchArgsErrors(restArgs: List<Expression>) {
    return !restArgs.isEmpty() && this.config.argsRest.isNone() ?
      [`Function "${this.name}" accepts ${this.config.args.size} args but got ${this.value.size}`] :
      [];
  }

  private getToFewArgsErrors(initialArgsCount: number, requiredArgsCount: number) {
    return initialArgsCount < requiredArgsCount ?
      [`Function "${this.name}" requires ${requiredArgsCount} args but got ${initialArgsCount}`] :
      [];
  }

  private getInitialArgsErrors(initialArgs: List<Expression>): string[] {
    return zip(
      this.config.args.slice(0, initialArgs.size).toArray(),
      initialArgs.toArray())
    .map(([config, argOption], index) => {
      const arg = Maybe.fromNull(argOption);
      const valid = arg.fold(false)(({ returnType }) => isSubtype(returnType, config.type));

      return ({ arg, config, index, valid });
    })
    .filter(({ valid }) => !valid)
    .map(( { arg, config, index }) =>
      `Function "${this.name}" has wrong arg passed: "${config.label}" ` +
      `(${toOrdinal(index + 1)} param) should be ${config.type} but ` +
      `is ${arg.fold('undefined')(({ returnType }) => returnType )}.`);
  }

  private getRestArgsErrors(restArgs: List<Expression>): string[] {
    return this.config.argsRest
      .map(({ type }) => restArgs
        .map((arg, index) => ({ arg, index, valid: isSubtype(arg.returnType, type) }))
        .filter(({ valid }) => !valid)
        .map(({ arg, index }) =>
          `Function "${this.name}" has wrong ${toOrdinal(index + 1)} rest arg passed, ` +
          `should be ${type} but is ${arg.returnType}`))
      .filter(errors => !errors.isEmpty())
      .fold([])(errors => errors.toArray());
  }

}
