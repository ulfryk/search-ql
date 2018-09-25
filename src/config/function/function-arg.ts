/* tslint:disable:max-classes-per-file */
import { Maybe, None } from 'monet';

import { ExpressionType, ValueType } from '../../common/model';
import { ArgDemand, IFunctionArg } from '../../dto';

abstract class FunctionArg {

  public static fromJS(_pojo: IFunctionArg): FunctionArg {
    throw Error('unimplemented');
  }

  public abstract readonly demand: ArgDemand;

  constructor(
    public readonly type: ValueType,
    public readonly label: string,
    public readonly expressionType: Maybe<ExpressionType[]> = None(), // non empty array
  ) {}

  public toJS(): IFunctionArg {
    return {
      demand: this.demand,
      expressionType: this.expressionType.orNull(),
      label: this.label,
      type: this.type,
    };
  }

  public toString() {
    return `ARG { ${this.label}${this.demand === ArgDemand.Optional ? '?' : ''}: ` +
      `${this.toTypeString()} }`;
  }

  public toTypeString() {
    return `${this.type}${this.expressionType.fold('')(et => `/(${et})`)}`;
  }

}

class OptionalFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new OptionalFunctionArg(type, label);
  }

  public readonly demand: ArgDemand.Optional = ArgDemand.Optional;

}

class RequiredFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new RequiredFunctionArg(type, label);
  }

  public readonly demand: ArgDemand.Required = ArgDemand.Required;

}

FunctionArg.fromJS = ({ demand, label, type }: IFunctionArg) => {
  switch (demand) {
    case ArgDemand.Optional: return OptionalFunctionArg.fromType(type, label);
    case ArgDemand.Required: return RequiredFunctionArg.fromType(type, label);
    default: throw Error(`No such ArgDemand value: ${demand}`);
  }
};

export { FunctionArg, OptionalFunctionArg, RequiredFunctionArg };
