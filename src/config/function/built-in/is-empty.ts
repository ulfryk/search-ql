import { List } from 'immutable';
import { None } from 'monet';

import { NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const isEmptyRuntime: FunctionRuntime<boolean> = (values, ast) => args =>
  NodeEvaluation.ofBoolean(values, ast)(String(args.first()().value).length === 0);

export const isEmptyFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
    None(),
    ValueType.Boolean,
    isEmptyRuntime);
