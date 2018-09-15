import { ISetoid } from '@samwise-tech/core';

import { OperatorAssociativity, OperatorType } from '../../common/model';
import { operatorAssociativity, operatorPrecedence, ParserConfig } from '../../config';
import { IOperator } from '../../dto';

export abstract class Operator implements ISetoid {

  public static fromToken(_config: ParserConfig): (token: string) => Operator {
    throw Error('unimplemented');
  }

  public static fromJS(_pojo: IOperator): Operator {
    throw Error('unimplemented');
  }

  public abstract readonly type: OperatorType;

  public get associativity(): OperatorAssociativity {
    return operatorAssociativity.get(this.type);
  }

  public get precedence(): number {
    return operatorPrecedence.get(this.type);
  }

  constructor(public readonly token: string) {}

  public is<C extends typeof Operator>(ctor: C) {
    return this instanceof ctor;
  }

  public equals(other: Operator): boolean {
    return this === other || other.is(this.constructor as typeof Operator);
  }

  public toString(): string {
    return this.token;
  }

  public inspect(): string {
    return this.toString();
  }

  public toJS(): IOperator {
    return {
      token: this.token,
      type: this.type,
    };
  }

}
