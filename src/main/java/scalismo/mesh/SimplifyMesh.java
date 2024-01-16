/*

    Mesh Simplification
    (C) by Sven Forstmann in 2014

    derived from: https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification
    and: https://github.com/timknip/mesh-decimate/blob/master/src/simplify.js

    License : MIT
    http://opensource.org/licenses/MIT

    Converted to java / jmonkeyengine by James Khan a.k.a jayfella

 */

package scalismo.mesh;

import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.VertexBuffer;
import com.jme3.scene.mesh.IndexBuffer;
import com.jme3.util.BufferUtils;

import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

/**
 * @author James Khan / jayfella
 */
public class SimplifyMesh {

    class SymetricMatrix {

        private final double m[] = new double[10];

        SymetricMatrix(double c) {

            for (int i = 0; i < 10; i++) {
                m[i] = c;
            }
        }

        SymetricMatrix(double m11, double m12, double m13, double m14,
                       double m22, double m23, double m24,
                       double m33, double m34,
                       double m44) {
            m[0] = m11;
            m[1] = m12;
            m[2] = m13;
            m[3] = m14;
            m[4] = m22;
            m[5] = m23;
            m[6] = m24;
            m[7] = m33;
            m[8] = m34;
            m[9] = m44;
        }

        // Make plane
        SymetricMatrix(double a, double b, double c, double d) {
            m[0] = a * a;
            m[1] = a * b;
            m[2] = a * c;
            m[3] = a * d;
            m[4] = b * b;
            m[5] = b * c;
            m[6] = b * d;
            m[7] = c * c;
            m[8] = c * d;
            m[9] = d * d;
        }

        void set(SymetricMatrix s) {
            System.arraycopy(s.m, 0, m, 0, m.length);
        }

        final double getValue(int c) {
            return m[c];
        }

        // Determinant
        final double det(int a11, int a12, int a13,
                         int a21, int a22, int a23,
                         int a31, int a32, int a33) {
            return m[a11] * m[a22] * m[a33] + m[a13] * m[a21] * m[a32] + m[a12] * m[a23] * m[a31]
                    - m[a13] * m[a22] * m[a31] - m[a11] * m[a23] * m[a32] - m[a12] * m[a21] * m[a33];
        }

        final SymetricMatrix add(final SymetricMatrix n) {
            return new SymetricMatrix(
                    m[0] + n.getValue(0),
                    m[1] + n.getValue(1),
                    m[2] + n.getValue(2),
                    m[3] + n.getValue(3),
                    m[4] + n.getValue(4),
                    m[5] + n.getValue(5),
                    m[6] + n.getValue(6),
                    m[7] + n.getValue(7),
                    m[8] + n.getValue(8),
                    m[9] + n.getValue(9));
        }

        void addLocal(final SymetricMatrix n) {
            m[0] += n.getValue(0);
            m[1] += n.getValue(1);
            m[2] += n.getValue(2);
            m[3] += n.getValue(3);
            m[4] += n.getValue(4);
            m[5] += n.getValue(5);
            m[6] += n.getValue(6);
            m[7] += n.getValue(7);
            m[8] += n.getValue(8);
            m[9] += n.getValue(9);

        }

    }

    class Vertex {

        private final Vector3f p = new Vector3f();
        private int tstart;
        private int tcount;
        private final SymetricMatrix q = new SymetricMatrix(0);
        private boolean border;

        Vertex(Vector3f p) {
            this.p.set(p);
        }


    }

    class Triangle {

        private final int v[] = new int[3];
        private final double[] err = new double[4];
        private boolean deleted = false;
        private boolean dirty = false;
        private final Vector3f n = new Vector3f();

        Triangle(int a, int b, int c) {
            this.v[0] = a;
            this.v[1] = b;
            this.v[2] = c;
        }

    }


    class Ref {

        private int tid;
        private int tvertex;

        Ref(int tid, int tvertex) {
            this.tid = tid;
            this.tvertex = tvertex;
        }

    }

    private Vector<Triangle> triangles = new Vector<>();
    private Vector<Vertex> vertices = new Vector<>();
    private Vector<Ref> refs = new Vector<>();

    private final Mesh inMesh;

    // used all over the place.
    private final Vector3f p = new Vector3f();

    public SimplifyMesh(Mesh mesh) {

        this.inMesh = mesh;

    }

    private Vector3f[] meshInVerts;
    private Vector3f[] meshInNorms;

    private int inMeshContainsVert(Vector3f vert) {
        for (int i = 0; i < meshInVerts.length; i++) {
            if (meshInVerts[i].equals(vert)) {
                return i;
            }
        }

        return -1;
    }

    private void readJmeMesh() {

        triangles.clear();
        vertices.clear();
        refs.clear();

        Vector3f[] meshVerts = BufferUtils.getVector3Array(this.inMesh.getFloatBuffer(VertexBuffer.Type.Position));
        meshInVerts = meshVerts;

        meshInNorms = BufferUtils.getVector3Array(this.inMesh.getFloatBuffer(VertexBuffer.Type.Normal));

        IndexBuffer ib = this.inMesh.getIndexBuffer();
        int[] meshIndices = new int[ib.size()];

        for (int i = 0; i < meshIndices.length; i++) {
            meshIndices[i] = ib.get(i);
        }

        for (Vector3f meshVert : meshVerts) {
            final Vertex v = new Vertex(meshVert);
            vertices.add(v);
        }

        int index = 0;
        int triIndex = 0;
        for (int i = 0; i < meshIndices.length; i += 3) {

            final Triangle t = new Triangle(
                    meshIndices[index++],
                    meshIndices[index++],
                    meshIndices[index++]
            );

            triangles.add(t);

            Ref ref1 = new Ref(triIndex, t.v[0]);
            Ref ref2 = new Ref(triIndex, t.v[1]);
            Ref ref3 = new Ref(triIndex, t.v[2]);

            refs.add(ref1);
            refs.add(ref2);
            refs.add(ref3);

            triIndex++;
        }
    }

    /**
     * Begins the simplification process.
     * @param target_percent the amount in percent to attempt to achieve. For example: 0.25f would result in creating
     *                       a mesh with 25% of triangles contained in the original.
     * @param agressiveness  sharpness to increase the threshold. 5..8 are good numbers. more iterations yield higher
     *                       quality. Minimum 4 and maximum 20 are recommended.
     * @param complexNormals Whether or not to generate computationally expensive normals, or just use the face normal.
     */
    public Mesh simplify(float target_percent, double agressiveness, boolean complexNormals) {

        int target_count = (int) (inMesh.getTriangleCount() * target_percent);
        return simplify(target_count, agressiveness, complexNormals);
    }

    /**
     * Begins the simplification process.
     * @param target_count  the amount of triangles to attempt to achieve.
     * @param agressiveness sharpness to increase the threshold. 5..8 are good numbers. more iterations yield higher
     *                      quality. Minimum 4 and maximum 20 are recommended.
     * @param complexNormals Whether or not to generate computationally expensive normals, or just use the face normal.
     */
    public Mesh simplify(int target_count, double agressiveness, boolean complexNormals) {

        // init

        // re-read the mesh every time we simplify to start with the original data.
        readJmeMesh();

        /*
        System.out.println(String.format("Simplify Target: %d of %d (%d%%)",
                target_count,
                triangles.size(),
                target_count * 100 / triangles.size()));

        final long timeStart = System.currentTimeMillis();
        */

        triangles.forEach(t -> t.deleted = false);

        // main iteration loop

        int deleted_triangles = 0;

        final Vector<Boolean> deleted0 = new Vector<>();
        final Vector<Boolean> deleted1 = new Vector<>();

        final int triangle_count = triangles.size();

        // final Vector3f p = new Vector3f();
        p.set(0, 0, 0);

        for (int iteration = 0; iteration < 1000; iteration++) {

            /*
            System.out.println(String.format(
                    "Iteration %02d -> triangles [ deleted: %d : count: %d | removed: %d%% ]",
                    iteration,
                    deleted_triangles,
                    triangle_count - deleted_triangles,
                    (deleted_triangles * 100 / triangle_count)
            ));
            */


            // target number of triangles reached ? Then break
            if (triangle_count - deleted_triangles <= target_count) {
                break;
            }

            // update mesh once in a while
            if (iteration % 5 == 0) {
                update_mesh(iteration);
            }

            // clear dirty flag
            triangles.forEach(t -> t.dirty = false);

            //
            // All triangles with edges below the threshold will be removed
            //
            // The following numbers works well for most models.
            // If it does not, try to adjust the 3 parameters
            //
            final double threshold = 0.000000001d * Math.pow(iteration + 3d, agressiveness);

            // remove vertices & mark deleted triangles
            for (int i = triangles.size() - 1; i >= 0; i--) {

                final Triangle t = triangles.get(i);

                if (t.err[3] > threshold || t.deleted || t.dirty) continue;

                for (int j = 0; j < 3; j++) {

                    if (t.err[j] >= threshold) {
                        continue;
                    }

                    final int i0 = t.v[j];
                    final int i1 = t.v[ ( j + 1 ) % 3];

                    final Vertex v0 = vertices.get(i0);
                    final Vertex v1 = vertices.get(i1);

                    // Border check
                    if (v0.border || v1.border) {
                        continue;
                    }

                    // Compute vertex to collapse to
                    // final Vector3f p = new Vector3f();
                    p.set(0, 0, 0);
                    calculate_error(i0, i1, p);

                    deleted0.setSize(v0.tcount); // normals temporarily
                    deleted1.setSize(v1.tcount); // normals temporarily
                    // deleted0.trimToSize();
                    // deleted1.trimToSize();

                    // don't remove if flipped
                    if (flipped(p, i1, v0, deleted0)) {
                        continue;
                    }

                    if (flipped(p, i0, v1, deleted1)) {
                        continue;
                    }

                    // not flipped, so remove edge
                    v0.p.set(p);
                    v0.q.addLocal(v1.q);

                    final int tstart = refs.size();

                    deleted_triangles += update_triangles(i0, v0, deleted0);
                    deleted_triangles += update_triangles(i0, v1, deleted1);

                    final int tcount = refs.size() - tstart;

                    v0.tstart = tstart;
                    v0.tcount = tcount;

                    break;
                }

                // done?
                if (triangle_count - deleted_triangles <= target_count) {
                    break;
                }

            }

        }

        // clean up mesh
        compact_mesh();

        // ready
        /*
        long timeEnd = System.currentTimeMillis();

        System.out.println(String.format("Simplify: %d/%d %d%% removed in %d ms",
                triangle_count - deleted_triangles,
                triangle_count,
                deleted_triangles * 100 / triangle_count,
                timeEnd-timeStart));
        */

        return createSimplifiedMesh(complexNormals);
    }

    // Check if a triangle flips when this edge is removed
    private boolean flipped(final Vector3f p, final int i1, final Vertex v0, final Vector<Boolean> deleted)
    {
        for (int k = 0; k < v0.tcount; k++)
        {
            Ref ref = refs.get(v0.tstart + k);
            Triangle t = triangles.get(ref.tid);

            if(t.deleted){
                continue;
            }

            final int s = ref.tvertex;
            final int id1 = t.v[(s + 1) % 3];
            final int id2 = t.v[(s + 2) % 3];

            if( id1 == i1 || id2 == i1) // delete ?
            {
                deleted.set(k, true);
                continue;
            }

            final Vector3f d1 = vertices.get(id1).p.subtract(p).normalizeLocal();
            final Vector3f d2 = vertices.get(id2).p.subtract(p).normalizeLocal();


            if(Math.abs(d1.dot(d2)) > 0.9999d) {
                return true;
            }

            final Vector3f n = new Vector3f(d1)
                    .crossLocal(d2)
                    .normalizeLocal();

            deleted.set(k, false);

            if(n.dot(t.n) < 0.2d) {
                return true;
            }
        }

        return false;
    }







    // Update triangle connections and edge error after a edge is collapsed
    private int update_triangles(final int i0, final Vertex v, final Vector<Boolean> deleted)
    {
        int tris_deleted = 0;

        // final Vector3f p = new Vector3f();
        p.set(0, 0, 0);

        for (int k = 0; k < v.tcount; k++) {

            final Ref r = refs.get(v.tstart + k);
            final Triangle t = triangles.get(r.tid);

            if(t.deleted){
                continue;
            }

            if(deleted.get(k)) {
                t.deleted = true;
                tris_deleted++;
                continue;
            }

            t.v[r.tvertex] = i0;
            t.dirty = true;
            t.err[0] = calculate_error(t.v[0], t.v[1], p);
            t.err[1] = calculate_error(t.v[1], t.v[2], p);
            t.err[2] = calculate_error(t.v[2], t.v[0], p);
            t.err[3] = Math.min(t.err[0], Math.min(t.err[1],t.err[2]));

            refs.add(r);
        }

        return tris_deleted;
    }

    private void update_mesh(final int iteration) {

        if(iteration > 0) { // compact triangles

            int dst = 0;

            for (int i = 0; i < triangles.size(); i++) {
                if(!triangles.get(i).deleted) {
                    triangles.set(dst++, triangles.get(i));
                }
            }

            triangles.setSize(dst);
        }

        //
        // Init Quadrics by Plane & Edge Errors
        //
        // required at the beginning ( iteration == 0 )
        // recomputing during the simplification is not required,
        // but mostly improves the result for closed meshes
        //
        if( iteration == 0 )
        {
            vertices.forEach(v -> v.q.set(new SymetricMatrix(0.0d)));

            // for (Triangle t : triangles) {
            triangles.forEach(t -> {

                Vector3f[] p = new Vector3f[] {
                        vertices.get(t.v[0]).p,
                        vertices.get(t.v[1]).p,
                        vertices.get(t.v[2]).p,
                };


                Vector3f n = p[1].subtract(p[0])
                        .crossLocal(p[2].subtract(p[0]))
                        .normalizeLocal();

                t.n.set(n);

                for (int j = 0; j < 3; j++) {
                    vertices.get(t.v[j]).q.set(
                            vertices.get(t.v[j]).q.add(new SymetricMatrix(n.x, n.y, n.z, -n.dot(p[0]))));
                }

            });

            // final Vector3f p = new Vector3f();
            p.set(0, 0, 0);

            triangles.forEach(t -> {

                for (int j = 0; j < 3; j++) {
                    t.err[j] = calculate_error(t.v[j], t.v[(j + 1) % 3], p);
                }

                t.err[3] = Math.min(t.err[0], Math.min(t.err[1], t.err[2]));

            });

        }

        // Init Reference ID list
        vertices.forEach(v -> { v.tstart = 0; v.tcount = 0; });

        triangles.forEach(t -> {
            vertices.get(t.v[0]).tcount++;
            vertices.get(t.v[1]).tcount++;
            vertices.get(t.v[2]).tcount++;
        });

        int tstart = 0;

        for (Vertex v : vertices) {
            v.tstart = tstart;
            tstart += v.tcount;
            v.tcount = 0;
        }

        // Write References
        refs.setSize(triangles.size() * 3);

        for (int i = 0; i < triangles.size(); i++) {

            Triangle t = triangles.get(i);

            for (int j = 0; j < 3; j++) {
                Vertex v = vertices.get(t.v[j]);
                refs.get(v.tstart + v.tcount).tid = i;
                refs.get(v.tstart + v.tcount).tvertex = j;
                v.tcount++;
            }
        }

        // Identify boundary : vertices[].border=0,1
        if( iteration == 0 )
        {
            final Vector<Integer> vcount = new Vector<>();
            final Vector<Integer> vids = new Vector<>();

            vertices.forEach(v -> v.border = false);

            vertices.forEach(v -> {

                vcount.clear();
                vids.clear();

                for (int j = 0; j < v.tcount; j++) {

                    int k = refs.get(v.tstart + j).tid;

                    Triangle t = triangles.get(k);

                    for (k = 0; k < 3; k++) {

                        int ofs = 0;

                        final int id = t.v[k];

                        while (ofs < vcount.size()) {
                            if (vids.get(ofs) == id) {
                                break;
                            }

                            ofs++;
                        }

                        if (ofs == vcount.size()) {
                            vcount.add(1);
                            vids.add(id);
                        } else {
                            vcount.set(ofs, vcount.get(ofs) + 1);
                        }
                    }
                }

                for (int j = 0; j < vcount.size(); j++) {
                    if (vcount.get(j) == 1) {
                        vertices.get(vids.get(j)).border = true;
                    }
                }


            });
        }
    }

    // Finally compact mesh before exiting
    private void compact_mesh()
    {
        int dst = 0;

        vertices.forEach(v -> v.tcount = 0);

        for (int i = 0; i < triangles.size(); i++) {
            if(!triangles.get(i).deleted) {
                Triangle t = triangles.get(i);

                triangles.set(dst++, t);

                for (int j = 0; j < 3; j++) {
                    vertices.get(t.v[j]).tcount = 1;
                }
            }
        }

        triangles.setSize(dst);

        dst = 0;

        for (Vertex vertice : vertices) {
            if (vertice.tcount != 0) {
                vertice.tstart = dst;
                vertices.get(dst).p.set(vertice.p);
                dst++;
            }
        }

        for (Triangle t : triangles) {
            for (int j = 0; j < 3; j++) {
                t.v[j] = vertices.get(t.v[j]).tstart;
            }
        }

        vertices.setSize(dst);
    }

    // Error between vertex and Quadric
    private double vertex_error(final SymetricMatrix q, final double x, final double y, final double z) {
        return    q.getValue(0) * x * x + 2
                * q.getValue(1)* x * y + 2
                * q.getValue(2) * x * z + 2
                * q.getValue(3) * x
                + q.getValue(4) * y * y + 2
                * q.getValue(5) * y * z + 2
                * q.getValue(6) * y
                + q.getValue(7) * z * z + 2
                * q.getValue(8) * z
                + q.getValue(9);
    }

    // Error for one edge
    private double calculate_error(final int id_v1, final int id_v2, final Vector3f p_result) {

        // compute interpolated vertex
        SymetricMatrix q = vertices.get(id_v1).q.add(vertices.get(id_v2).q);
        boolean   border = vertices.get(id_v1).border & vertices.get(id_v2).border;
        double error;
        double det = q.det(0, 1, 2, 1, 4, 5, 2, 5, 7);

        if ( det != 0 && !border )
        {
            // q_delta is invertible
            p_result.x = (float) (-1 / det*(q.det(1, 2, 3, 4, 5, 6, 5, 7 , 8)));	// vx = A41/det(q_delta)
            p_result.y = (float) (1 / det*(q.det(0, 2, 3, 1, 5, 6, 2, 7 , 8)));	// vy = A42/det(q_delta)
            p_result.z = (float) (-1 / det*(q.det(0, 1, 3, 1, 4, 6, 2, 5,  8)));	// vz = A43/det(q_delta)
            error = vertex_error(q, p_result.x, p_result.y, p_result.z);
        }
        else
        {
            // det = 0 -> try to find best result
            Vector3f p1 = vertices.get(id_v1).p;
            Vector3f p2 = vertices.get(id_v2).p;
            Vector3f p3 = p1.add(p2).divide(2.0f); // (p1+p2)/2;
            double error1 = vertex_error(q, p1.x, p1.y, p1.z);
            double error2 = vertex_error(q, p2.x, p2.y, p2.z);
            double error3 = vertex_error(q, p3.x, p3.y, p3.z);

            error = Math.min(error1, Math.min(error2, error3));

            if (error1 == error) p_result.set(p1);
            if (error2 == error) p_result.set(p2);
            if (error3 == error) p_result.set(p3);
        }

        return error;
    }


    private Mesh createSimplifiedMesh(boolean complexNormals) {

        Mesh mesh = new Mesh();

        Vector3f[] vertArray = new Vector3f[vertices.size()];

        for (int i = 0; i < vertArray.length; i++) {
            Vertex v = vertices.get(i);
            vertArray[i] = v.p;
        }

        List<Integer> indexList = new ArrayList<>();

        Vector3f[] normArray = complexNormals
                ? normalizeMesh(triangles, vertices)
                : new Vector3f[vertArray.length];

        triangles.forEach(t -> {
            indexList.add(t.v[0]);
            indexList.add(t.v[1]);
            indexList.add(t.v[2]);

            if (!complexNormals) {
                normArray[t.v[0]] = t.n.clone();
                normArray[t.v[1]] = t.n.clone();
                normArray[t.v[2]] = t.n.clone();
            }

        });

        int[] indexArray = new int[indexList.size()];

        for (int i = 0; i < indexList.size(); i++) {
            indexArray[i] = indexList.get(i);
        }

        /*
        System.out.println("Simplified mesh: ");
        System.out.println("\tVerts: " + vertArray.length + " of " + inMesh.getVertexCount());
        System.out.println("\tTris: " + (indexArray.length / 3) + " of " + inMesh.getTriangleCount());
        */

        FloatBuffer pb = BufferUtils.createFloatBuffer(vertArray);
        mesh.setBuffer(VertexBuffer.Type.Position, 3, pb);

        FloatBuffer nb = BufferUtils.createFloatBuffer(normArray);
        mesh.setBuffer(VertexBuffer.Type.Normal, 3, nb);

        IntBuffer ib = BufferUtils.createIntBuffer(indexArray);
        mesh.setBuffer(VertexBuffer.Type.Index, 3, ib);

        mesh.updateBound();

        return mesh;
    }

    private Vector3f[] normalizeMesh( Vector<Triangle> triangles, List<Vertex> verts )
    {
        Vector3f[] norms = new Vector3f[verts.size()];

        for( int i=0; i < verts.size(); i++ ) {
            norms[i] = new Vector3f();
        }

        for (Triangle face : triangles) {

            final int ia = face.v[0];
            final int ib = face.v[1];
            final int ic = face.v[2];

            final Vector3f e1 = verts.get(ia).p.subtract(verts.get(ib).p);
            final Vector3f e2 = verts.get(ic).p.subtract(verts.get(ib).p);
            final Vector3f no = e2.cross(e1); //cross( e1, e2 );

            norms[ia].addLocal(no);
            norms[ib].addLocal(no);
            norms[ic].addLocal(no);

        }

        for (Vector3f norm : norms) {
            norm.normalizeLocal();
        }

        for (int v = 0; v < verts.size(); v++) {

            int index = inMeshContainsVert(verts.get(v).p);

            if (index != -1) {
                norms[v] = meshInNorms[index];
            }

        }

        return norms;

    }

    /**
     * The mesh that was given in the constructor.
     * @return the original untouched mesh given in the constructor.
     */
    public Mesh getOriginalMesh() {
        return this.inMesh;
    }

}
