#!/usr/bin/env python3
"""Generate small, redistributable PDF fixtures into data/fixtures/.

These are authored from scratch (no third-party content), so unlike
data/sample they can live in the public repository and run in CI.

Fixtures:
  classic.pdf     - traditional xref table
  xrefstream.pdf  - PDF 1.5 cross-reference stream (uncompressed)
  incremental.pdf - classic body plus one incremental update (/Prev chain)
"""

import os

FIXDIR = os.path.join(os.path.dirname(__file__), "..", "data", "fixtures")


def obj(num, body):
    return f"{num} 0 obj\n{body}\nendobj\n".encode()


def content_stream(text):
    stream = f"BT /F1 24 Tf 72 720 Td ({text}) Tj ET".encode()
    return (
        f"<< /Length {len(stream)} >>\nstream\n".encode()
        + stream
        + b"\nendstream"
    )


def base_objects(text):
    return {
        1: b"<< /Type /Catalog /Pages 2 0 R >>",
        2: b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        3: (
            b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
            b"/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"
        ),
        4: content_stream(text),
        5: b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>",
    }


def build_body(objects):
    """Serialize objects, returning (bytes, {objnum: offset})."""
    out = b"%PDF-1.5\n%\xc2\xb5\xc2\xb6\n"
    offsets = {}
    for num in sorted(objects):
        offsets[num] = len(out)
        out += obj(num, objects[num].decode("latin1"))
    return out, offsets


def xref_table(offsets, size):
    lines = [b"xref\n", f"0 {size}\n".encode(), b"0000000000 65535 f \n"]
    for num in range(1, size):
        lines.append(f"{offsets[num]:010d} 00000 n \n".encode())
    return b"".join(lines)


def classic():
    body, offsets = build_body(base_objects("Hello classic xref"))
    xref_pos = len(body)
    out = body + xref_table(offsets, 6)
    out += (
        b"trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n"
        + str(xref_pos).encode()
        + b"\n%%EOF\n"
    )
    return out


def xrefstream():
    objects = base_objects("Hello xref stream")
    body, offsets = build_body(objects)
    xref_pos = len(body)
    # W [1 2 2]: type(1) offset(2) generation(2), objects 0..6
    entries = b"\x00\x00\x00\xff\xff"  # object 0: free
    for num in range(1, 6):
        entries += b"\x01" + offsets[num].to_bytes(2, "big") + b"\x00\x00"
    entries += b"\x01" + xref_pos.to_bytes(2, "big") + b"\x00\x00"  # obj 6: this stream
    dict_part = (
        f"<< /Type /XRef /Size 7 /W [1 2 2] /Root 1 0 R "
        f"/Length {len(entries)} >>"
    ).encode()
    out = body + b"6 0 obj\n" + dict_part + b"\nstream\n" + entries + b"\nendstream\nendobj\n"
    out += b"startxref\n" + str(xref_pos).encode() + b"\n%%EOF\n"
    return out


def incremental():
    base = classic()
    first_xref = int(base.rsplit(b"startxref\n", 1)[1].split(b"\n", 1)[0])
    update_offset = len(base)
    new_content = content_stream("Hello incremental update")
    out = base + obj(4, new_content.decode("latin1"))
    xref_pos = len(out)
    out += b"xref\n4 1\n" + f"{update_offset:010d} 00000 n \n".encode()
    out += (
        b"trailer\n<< /Size 6 /Root 1 0 R /Prev "
        + str(first_xref).encode()
        + b" >>\nstartxref\n"
        + str(xref_pos).encode()
        + b"\n%%EOF\n"
    )
    return out


def main():
    os.makedirs(FIXDIR, exist_ok=True)
    for name, data in [
        ("classic.pdf", classic()),
        ("xrefstream.pdf", xrefstream()),
        ("incremental.pdf", incremental()),
    ]:
        path = os.path.join(FIXDIR, name)
        with open(path, "wb") as f:
            f.write(data)
        print(f"wrote {path} ({len(data)} bytes)")


if __name__ == "__main__":
    main()
