import { List } from 'immutable';
import { None } from 'monet';

import { NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const lengthRuntime: FunctionRuntime<number> = (values, ast) => args =>
  NodeEvaluation.ofNumber(values, ast)(String(args.first()().value).length);

export const lengthFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
    None(),
    ValueType.Number,
    lengthRuntime);
