package opmodes.test;
import android.content.Context;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;

import org.firstinspires.ftc.robotcore.internal.system.AppUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.StandardFourMotorRobot;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "XX Test")
@Disabled
public class XXTest extends StandardFourMotorRobot {

    // drivetrain and mechanisms declaration
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
        super.init();

        // drivetrain and mechanisms initialization
        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontRight);

        xxPath = new DeadReckonPath();
        xxPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, 0.5);
        xxPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, -0.5);

        Context context = AppUtil.getDefContext();
        try {
            File path = context.getExternalFilesDir(null);
            file = new File(path, "backward.txt");
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

    public void initialMove(DeadReckonPath path)
    {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        try {
                            drivetrain.stop();
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

        int frontLeftEncoder = Math.abs(frontLeft.getCurrentPosition());
        int frontRightEncoder = Math.abs(frontRight.getCurrentPosition());
        int backLeftEncoder = Math.abs(backLeft.getCurrentPosition());
        int backRightEncoder = Math.abs(backRight.getCurrentPosition());

        try {
            stream.write(String.format("%d,%d,%d,%d\n", frontLeftEncoder, frontRightEncoder, backLeftEncoder, backRightEncoder).getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
