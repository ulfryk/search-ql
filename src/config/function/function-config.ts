import { ISetoid } from '@samwise-tech/core';
import { List } from 'immutable';
import { Maybe } from 'monet';

import { ValueType } from '../../common/model';
import { FunctionArg, OptionalFunctionArg } from './function-arg';

export class FunctionConfig implements ISetoid {

  constructor(
    public readonly name: string,
    public readonly args: List<FunctionArg>,
    public readonly argsRest: Maybe<OptionalFunctionArg>,
    public readonly returnType: ValueType,
  ) {}

  public equals(other: FunctionConfig) {
    return this === other || (
      this.name === other.name &&
      this.args.equals(other.args) &&
      this.returnType === other.returnType
    );
  }

}
