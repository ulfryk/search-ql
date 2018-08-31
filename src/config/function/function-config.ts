import { ISetoid } from '@samwise-tech/core';
import { List } from 'immutable';

import { ValueType } from '../../common/model';
import { FunctionRuntime } from '../../common/runtimes';

export class FunctionConfig implements ISetoid {

  constructor(
    public readonly name: string,
    public readonly argTypes: List<ValueType>, // maybe Map<ArgLabel, ArgType> ?
    public readonly returnType: ValueType,
    public readonly runtime: FunctionRuntime,
  ) {}

  public equals(other: FunctionConfig) {
    return this === other || (
      this.name === other.name &&
      this.argTypes.equals(other.argTypes) &&
      this.returnType === other.returnType
    );
  }

}
