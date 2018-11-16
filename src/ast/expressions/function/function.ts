import { Bind } from '@samwise-tech/core';
import { List, Map, Set } from 'immutable';
import { zip } from 'lodash';
import { Maybe, Some } from 'monet';

import { checkBoolCompatibility, Expression, ExpressionType, IntegrityFailure, isSubtype, TypeFailure, ValueType } from '../../../common/model';
import { toOrdinal } from '../../../common/utils';
import { FunctionConfig, RequiredFunctionArg } from '../../../config';
import { IFunctionExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';
import { DateExpression, NumberExpression, PhraseExpression, SelectorExpression, TermExpression, TextExpression } from '../term';

export class FunctionExpression extends Expression {

  public static fromParseResult(config: FunctionConfig, args: Expression[]) {
    const argsCount = args.length;
    const baseArgsCount = config.args.size;
    const restArgsCount = argsCount - baseArgsCount;
    const absRestCount = restArgsCount > 0 ? restArgsCount : 0;
    const argExpressions = List(config.args)
      .map(({ type }) => Some(type))
      // tslint:disable-next-line:no-array-mutation
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
                return arg.is(SelectorExpression as any) ? arg :
                  TextExpression.fromTerm(arg as TermExpression);
              case ValueType.Phrase:
                return PhraseExpression.fromTerm(arg as TermExpression);
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

  public readonly type: ExpressionType.Function = ExpressionType.Function;

  constructor(
    public readonly value: List<Expression>,
    public readonly config: FunctionConfig,
  ) {
    super();
    Bind.to(this);
  }

  public get name() {
    return this.config.name;
  }

  public get returnType(): ValueType {
    return this.config.returnType.cata(this.deriveReturnTypeFromArgs, __ => __);
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

  public checkTypes(): Expression {
    return this.getTypeErrors()
      .map(errors => errors.map(TypeFailure.fromError(this)))
      .foldLeft(this.clone(this.value.map(arg => arg.checkTypes()).toList()))(
        InvalidExpression.fromErrors);
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    return this.getIntegrityErrors(model)
      .map(errors => errors.map(IntegrityFailure.fromError(this)))
      .foldLeft(this.clone(this.value.map(arg => arg.checkIntegrity(model)).toList()))(
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

  public toJS(): IFunctionExpression {
    return {
      config: this.config.toJS(),
      returnType: this.returnType,
      type: this.type,
      value: this.value.map(arg => arg.toJS()).toArray(),
    };
  }

  private clone(args: List<Expression>): Expression {
    if (args.equals(this.value)) {
      return this;
    }

    return new FunctionExpression(args, this.config);
  }

  @Bind private deriveReturnTypeFromArgs(returnTypeParam: string): ValueType {
    return Map<string, ValueType>(this.getGenericParams()).get(returnTypeParam);
  }

  private getGenericParams(): [string, ValueType][] {
    const restSize = this.config.argsRest.fold(0)(() =>
      this.value.size > this.config.args.size ?
        this.value.size - this.config.args.size : 0);
    // tslint:disable-next-line:no-array-mutation
    const restArgs = this.config.argsRest.fold([])(arg => Array(restSize).fill(arg));
    const argConfigs = this.config.args.concat(restArgs).toArray();

    return zip(argConfigs, this.value.toArray())
      .filter(([config, arg]) => Boolean(config) && Boolean(arg))
      .filter(([config, _arg]) => config.typeParam.isSome())
      .map(([config, arg]) => [config.typeParam.some(), arg.returnType] as [string, ValueType]);
  }

  // --- type check ---

  private getTypeErrors(): Maybe<string[]> {
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
      ...this.getWrongTypeParamsErrors(),
    ]).filter(errors => errors.length > 0);
  }

  private getWrongTypeParamsErrors(): string[] {
    return List(this.getGenericParams())
      .groupBy(([param]) => param)
      .map<Set<ValueType>>(group => Set(group.map(([_p, type]) => type)))
      .filter(group => group.size > 1)
      .map<[string, ValueType[]]>((group, param: string) => [param, group.toArray()])
      .toArray()
      .map(([param, types]: [string, ValueType[]]) =>
        `Each type param should match exactly one type but ${param} ` +
        `has multiple resolutions: ${types.join(', ')}`);
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
      // tslint:disable-next-line:cyclomatic-complexity
      const valid = arg.fold(false)(argExpression => {
        const argType = argExpression.is(SelectorExpression as any) ?
          (argExpression as SelectorExpression).matchingType : argExpression.returnType;

        return (
          isSubtype(argType, config.type) ||
          checkBoolCompatibility(argType, config.type)
        ) && config.expressionType.orJust([argExpression.type]).includes(argExpression.type);
      });

      return ({ arg, config, index, valid });
    })
    .filter(({ valid }) => !valid)
    .map(( { arg, config, index }) =>
      `Function "${this.name}" has wrong arg passed: "${config.label}" ` +
      `(${toOrdinal(index + 1)} param) should be ${config.toTypeString()} but ` +
      `is ${arg.fold('not defined')(expr => expr.toTypeString())}.`);
  }

  private getRestArgsErrors(restArgs: List<Expression>): string[] {
    return this.config.argsRest
      .map(config => restArgs
        .map((arg, index) => ({
          arg,
          index,
          validExpressionType: config.expressionType
            .orJust([arg.type])
            .includes(arg.type),
          validReturnType: isSubtype(arg.returnType, config.type) ||
            checkBoolCompatibility(arg.returnType, config.type),
        }))
        .filter(({ validExpressionType, validReturnType }) =>
          !validExpressionType || !validReturnType)
        .map(({ arg, index }) =>
          `Function "${this.name}" has wrong ${toOrdinal(index + 1)} rest arg passed, ` +
          `should be ${config.toTypeString()} but is ${arg.toTypeString()}`))
      .filter(errors => !errors.isEmpty())
      .fold([])(errors => errors.toArray());
  }

  // --- integrity check ---

  private getIntegrityErrors(_model: Map<string, ValueType>): Maybe<string[]> {
    return this.config.checkIntegrity();
  }

}
