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
    public readonly typeParam: Maybe<string>,
    public readonly expressionType: Maybe<ExpressionType[]> = None(), // non empty array
  ) {}

  public toJS(): IFunctionArg {
    return {
      demand: this.demand,
      expressionType: this.expressionType.orNull(),
      label: this.label,
      type: this.type,
      typeParam: this.typeParam.orNull(),
    };
  }

  public toString() {
    return `ARG { ${this.label}${this.demand === ArgDemand.Optional ? '?' : ''}: ` +
      `${this.toTypeString()} }`;
  }

  public toTypeString() {
    const expressionTypeSuffix = this.expressionType.fold('')(et => `/(${et})`);

    return this.typeParam.fold(`${this.type}${expressionTypeSuffix}`)(
      param => `${param}${expressionTypeSuffix}`);
  }

}

class OptionalFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string, typeParam?: string | null) {
    return new OptionalFunctionArg(type, label, Maybe.fromNull(typeParam));
  }

  public readonly demand: ArgDemand.Optional = ArgDemand.Optional;

}

class RequiredFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string, typeParam?: string | null) {
    return new RequiredFunctionArg(type, label, Maybe.fromNull(typeParam));
  }

  public readonly demand: ArgDemand.Required = ArgDemand.Required;

}

FunctionArg.fromJS = ({ demand, label, type, typeParam }: IFunctionArg) => {
  switch (demand) {
    case ArgDemand.Optional: return OptionalFunctionArg.fromType(type, label, typeParam);
    case ArgDemand.Required: return RequiredFunctionArg.fromType(type, label, typeParam);
    default: throw Error(`No such ArgDemand value: ${demand}`);
  }
};

export { FunctionArg, OptionalFunctionArg, RequiredFunctionArg };
