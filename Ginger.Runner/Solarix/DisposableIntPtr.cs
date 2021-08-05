using System;
using JetBrains.Annotations;

namespace Ginger.Runner.Solarix
{
    internal sealed class DisposableIntPtr : IDisposable 
    {
        public DisposableIntPtr(IntPtr handle, Action<IntPtr> dispose, string objectName, IntPtr? nullValue = null)
        {
            CheckHandle(handle, objectName, nullValue);
            _handle = handle;
            _dispose = dispose;
            _objectName = objectName;
        }

        public void Dispose()
        {
            if (_isDisposed)
            {
                return;
            }

            _dispose(_handle);
            _isDisposed = true;
        }

        public static implicit operator IntPtr(DisposableIntPtr @this) =>
            !@this._isDisposed 
                ? @this._handle
                : throw new ObjectDisposedException(@this._objectName);

        [AssertionMethod]
        private static void CheckHandle(IntPtr handle, string objectName, IntPtr? nullValue)
        {
            if (handle == (nullValue ?? IntPtr.Zero))
            {
                throw new ArgumentNullException(nameof(handle), "Could not create " + objectName);
            }
        }

        private readonly IntPtr _handle;
        private readonly Action<IntPtr> _dispose;
        private readonly string _objectName;
        private bool _isDisposed;
    }
}
