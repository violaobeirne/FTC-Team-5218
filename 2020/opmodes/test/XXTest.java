package opmodes.test;
import android.content.Context;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import org.firstinspires.ftc.robotcore.internal.system.AppUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "XX Test")
public class XXTest extends Robot {

    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private MechanumGearedDrivetrain drivetrain;
    private DeadReckonPath xxPath;

    private File file;
    FileOutputStream stream;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init ()
    {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");

        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        xxPath = new DeadReckonPath();
        xxPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, 0.2);
        xxPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, -0.2);

        Context context = AppUtil.getDefContext();
        try {
            File path = context.getExternalFilesDir(null);
            file = new File(path, "sensor_data.txt");
            stream = new FileOutputStream(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

    }

    @Override
    public void start ()
    {
        initialMove(xxPath);
    }

    public void initialMove (DeadReckonPath path)
    {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        try {
                            stream.close();
                        } catch (IOException ex) {
                            ex.printStackTrace();
                        }
                        break;
                }
            }
        });
    }

    @Override
    public void loop ()
    {
        super.loop();

        int frontLeftEncoder = frontLeft.getCurrentPosition();
        int frontRightEncoder = frontRight.getCurrentPosition();
        int backLeftEncoder = backLeft.getCurrentPosition();
        int backRightEncoder = backRight.getCurrentPosition();

        try {
            stream.write(String.format("%d,%d,%d,%d\n", frontLeftEncoder, frontRightEncoder, backLeftEncoder, backRightEncoder).getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }


    }
}
