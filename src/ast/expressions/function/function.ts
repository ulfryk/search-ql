import { List } from 'immutable';
import { zip } from 'lodash';
import { Maybe, Some } from 'monet';

import { paramTypes } from '../../../common/model';
import { FunctionConfig, RequiredFunctionArg } from '../../../config';
import { Expression } from '../expression';
import { InvalidExpression } from '../invalid';

export class FunctionExpression extends Expression {

  public static fromParseResult(config: FunctionConfig, args: Expression[]) {
    return new FunctionExpression(List(args), config);
  }

  constructor(
    public readonly value: List<Expression>,
    public readonly config: FunctionConfig,
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
    return this.getError()
      .foldLeft(this.clone(this.value.map(arg => arg.checkTypes()).toList()))(
        InvalidExpression.fromError);
  }

  public toString() {
    return `${this.name}( ${this.value.join(' , ')} )`;
  }

  private clone(args: List<Expression>): Expression {
    if (args.equals(this.value)) {
      return this;
    }

    return new FunctionExpression(args, this.config);
  }

  // tslint:disable-next-line:cyclomatic-complexity
  private getError(): Maybe<string> {
    const restArgs = this.value.slice(this.config.args.size);

    if (!restArgs.isEmpty() && this.config.argsRest.isNone()) {
      return Some(
        `Function "${this.name}" accepts ${this.config.args.size} args but got ${this.value.size}`);
    }

    const initialArgs = this.value.slice(0, this.config.args.size);
    const requiredArgs = this.config.args.filter(arg => arg instanceof RequiredFunctionArg);

    if (initialArgs.size < requiredArgs.size) {
      return Some(
        `Function "${this.name}" requires ${requiredArgs.size} args but got ${initialArgs.size}`);
    }

    const invalidInitialArgs = zip(
        this.config.args.slice(0, initialArgs.size).toArray(),
        initialArgs.toArray())
      .filter(([config, arg]) => !paramTypes.get(config.type)(arg.returnType))
      .map(([config, arg]) =>
        `"${config.label}" should be ${config.type} but is ${arg.returnType}`);

    if (invalidInitialArgs.length > 0) {
      return Some(`Function "${this.name}" has wrong args passed: ${invalidInitialArgs.join('; ')}`);
    }

    const invalidRestArgs = this.config.argsRest
      .map(({ type }) => restArgs
        .filter(arg => !paramTypes.get(type)(arg.returnType))
        .map(arg =>
          `rest arg should be ${type} but is ${arg.returnType}`))
      .filter(errors => !errors.isEmpty());

    return invalidRestArgs.map(errors =>
      `Function "${this.name}" has wrong rest args passed: ${errors.join('; ')}`);
  }

}
