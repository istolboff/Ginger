using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;

namespace Prolog.Engine.Miscellaneous
{
    internal static class MayBe
    {
        public static MayBe<T> Some<T>(T value) => 
            new (value, true);

        public static MayBe<T> MakeNone<T>() => None;

        public static syntacticshugar_NoneProducer None => 
            new ();

		public static MayBe<TValue2> SelectMany<TValue, TIntermediate, TValue2>(
            this MayBe<TValue> @this,
            Func<TValue, MayBe<TIntermediate>> selector,
            Func<TValue, TIntermediate, TValue2> projector) 
        =>
			@this.HasValue && selector(@this.Value!) is var intermediate && intermediate.HasValue
				? Some(projector(@this.Value!, intermediate.Value!))
				: None;
		
		public static MayBe<IReadOnlyCollection<T>> Sequence<T>(IReadOnlyCollection<MayBe<T>> sequence) 
        =>
			sequence.Any(it => !it.HasValue)
				? None
				: Some(sequence.ConvertAll(it => it.Value!));
    }

    internal sealed record MayBe<T>(T? Value, bool HasValue)
    {
        public TResult Fold<TResult>(Func<T, TResult> convert, Func<TResult> getDefaultValue) =>
            HasValue ? convert(Value!) : getDefaultValue();

        public MayBe<TProjected> Map<TProjected>(Func<T, TProjected> f) => 
            new (HasValue ? f(Value!) : default(TProjected), HasValue);

        public T OrElse(T defaultValue) => 
            HasValue ? Value! : defaultValue;

        public T OrElse(Func<T> defaultValue) => 
            HasValue ? Value! : defaultValue();

        public override string? ToString() => 
            HasValue ? Value!.ToString() : "null";

#pragma warning disable CA2225 // Provide a method named 'ToEither' as an alternate for operator op_Implicit
#pragma warning disable CA1801 // Review unused parameters
        public static implicit operator MayBe<T>([UsedImplicitly] syntacticshugar_NoneProducer unused) =>
#pragma warning restore CA1801 
            new (default, false);
#pragma warning restore CA2225

        public static bool operator == (MayBe<T> @this, T value) =>
            @this
                .Map(v => EqualityComparer<T>.Default.Equals(v, value))
                .OrElse(false);

        public static bool operator != (MayBe<T> @this, T value) =>
            !(@this == value!);

        public static implicit operator MayBe<T>(MayBe<MayBe<T>> nestedMayBe) =>
            nestedMayBe.HasValue && nestedMayBe.Value!.HasValue 
                ? MayBe.Some(nestedMayBe.Value!.Value!) 
                : MayBe.None;
    }

#pragma warning disable CA1707 // Remove the underscores from type name
// ReSharper disable InconsistentNaming
    internal sealed record syntacticshugar_NoneProducer;
// ReSharper restore InconsistentNaming    
#pragma warning restore CA1707
}