import { List } from 'immutable';
import { Right, Some } from 'monet';

import { ValueType } from '../../../../common/model';
import { OptionalFunctionArg, RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const concatFunction =
  new FunctionConfig(
    'concat',
    List([
      RequiredFunctionArg.fromType(ValueType.Text, 'val1'),
      RequiredFunctionArg.fromType(ValueType.Text, 'val2'),
    ]),
    Some(OptionalFunctionArg.fromType(ValueType.Text, 'val2')),
    Right(ValueType.Text));
